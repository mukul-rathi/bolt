#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"

IRCodegenVisitor::IRCodegenVisitor() {
  context = llvm::make_unique<llvm::LLVMContext>();
  builder = std::unique_ptr<llvm::IRBuilder<>>(new llvm::IRBuilder<>(*context));
  module = llvm::make_unique<llvm::Module>("Module", *context);
}
void IRCodegenVisitor::dumpLLVMIR() { module->print(llvm::outs(), nullptr); }

std::string IRCodegenVisitor::dumpLLVMIRToString() {
  std::string outstr;
  llvm::raw_string_ostream oss(outstr);

  module->print(oss, nullptr);

  return oss.str();
}

void IRCodegenVisitor::codegenMainExpr(
    const std::vector<std::unique_ptr<ExprIR>> &mainExpr) {
  llvm::FunctionType *mainType =
      llvm::FunctionType::get(llvm::IntegerType::getInt32Ty(*context),
                              std::vector<llvm::Type *>(), false /* isVarArgs */
      );
  llvm::Function *main = llvm::Function::Create(
      mainType, llvm::Function::ExternalLinkage, "main", module.get());
  llvm::BasicBlock *mainBasicBlock =
      llvm::BasicBlock::Create(*context, "entry", main);
  builder->SetInsertPoint(mainBasicBlock);
  varEnv.clear();  // clear variables env

  for (auto &expr : mainExpr) {
    expr->codegen(*this);
  }

  llvm::APInt retVal(32 /* bitSize */, (uint32_t)0, true /* signed */);
  builder->CreateRet(llvm::ConstantInt::get(*(context), retVal));
  llvm::verifyFunction(*main);
}

void IRCodegenVisitor::codegenProgram(const ProgramIR &program) {
  codegenExternFunctionDeclarations();
  codegenClasses(program.classDefns);
  codegenFunctionProtos(program.functionDefns);
  codegenVTables(program.classDefns);
  codegenFunctionDefns(program.functionDefns);
  codegenMainExpr(program.mainExpr);
  runOptimisingPasses(program.functionDefns);
}

void IRCodegenVisitor::configureTarget() {
  auto TargetTriple = llvm::sys::getDefaultTargetTriple();
  module->setTargetTriple(TargetTriple);
}

void IRCodegenVisitor::runOptimisingPasses(
    const std::vector<std::unique_ptr<FunctionIR>> &functions) {
  std::unique_ptr<llvm::legacy::FunctionPassManager> functionPassManager =
      llvm::make_unique<llvm::legacy::FunctionPassManager>(module.get());

  // Promote allocas to registers.
  functionPassManager->add(llvm::createPromoteMemoryToRegisterPass());
  // Do simple "peephole" optimizations
  functionPassManager->add(llvm::createInstructionCombiningPass());
  // Reassociate expressions.
  functionPassManager->add(llvm::createReassociatePass());
  // Eliminate Common SubExpressions.
  functionPassManager->add(llvm::createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks etc).
  functionPassManager->add(llvm::createCFGSimplificationPass());

  functionPassManager->doInitialization();

  for (auto &function : functions) {
    llvm::Function *llvmFun =
        module->getFunction(llvm::StringRef(function->functionName));
    functionPassManager->run(*llvmFun);
  }

  llvm::Function *llvmMainFun = module->getFunction(llvm::StringRef("main"));
  functionPassManager->run(*llvmMainFun);
}
