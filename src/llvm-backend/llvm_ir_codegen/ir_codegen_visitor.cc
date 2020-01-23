#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"

IRCodegenVisitor::IRCodegenVisitor() {
  context = llvm::make_unique<llvm::LLVMContext>();
  builder = std::unique_ptr<llvm::IRBuilder<>>(new llvm::IRBuilder<>(*context));
  module = llvm::make_unique<llvm::Module>("Module", *context);
}

void IRCodegenVisitor::dumpLLVMIR() { module->print(llvm::outs(), nullptr); }

void IRCodegenVisitor::codegenProgram(const ProgramIR &program) {
  codegenClasses(program.classDefns);
  codegenFunctions(program.functionDefns);

  // Create the main function.
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

  for (auto &expr : program.mainExpr) {
    expr->accept(*this);
  }

  llvm::APInt retVal(32 /* bitSize */, (uint32_t)0, true /* signed */);
  builder->CreateRet(llvm::ConstantInt::get(*(context), retVal));
  llvm::verifyFunction(*main);
}