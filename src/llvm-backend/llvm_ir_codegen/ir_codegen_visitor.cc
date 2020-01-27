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
    expr->accept(*this);
  }

  llvm::APInt retVal(32 /* bitSize */, (uint32_t)0, true /* signed */);
  builder->CreateRet(llvm::ConstantInt::get(*(context), retVal));
  llvm::verifyFunction(*main);
}

void IRCodegenVisitor::codegenPThreads() {
  // pthread type varies in size depending on machine, for now hard-coded
  llvm::Type *pthreadTy = llvm::StructType::create(
      *context, llvm::StringRef("struct._opaque_pthread_t"));
  // void * represented as i8*
  llvm::Type *voidPtrTy = llvm::Type::getInt8Ty(*context)->getPointerTo();

  // (void *) fn (void * arg)
  llvm::FunctionType *funVoidPtrVoidTy = llvm::FunctionType::get(
      voidPtrTy, llvm::ArrayRef<llvm::Type *>({voidPtrTy}),
      /* has variadic args */ false);

  // int pthread_create(pthread_t * thread, const pthread_attr_t * attr, \
  //                  void * (*start_routine)(void *), void * arg)
  llvm::FunctionType *pthreadCreateTy = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(*context),
      llvm::ArrayRef<llvm::Type *>({pthreadTy->getPointerTo(), voidPtrTy,
                                    (funVoidPtrVoidTy)->getPointerTo(),
                                    voidPtrTy}),
      /* has variadic args */ false);
  module->getOrInsertFunction("pthread_create", pthreadCreateTy);

  // int pthread_join(pthread_t thread, void **value_ptr)

  llvm::FunctionType *pthreadJoinTy = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(*context),
      llvm::ArrayRef<llvm::Type *>(
          {pthreadTy->getPointerTo(), voidPtrTy->getPointerTo()}),
      /* has variadic args */ false);
  module->getOrInsertFunction("pthread_join", pthreadJoinTy);
}
void IRCodegenVisitor::codegenProgram(const ProgramIR &program) {
  codegenPThreads();
  codegenClasses(program.classDefns);
  codegenFunctions(program.functionDefns);
  codegenMainExpr(program.mainExpr);
}

void IRCodegenVisitor::configureTarget() {
  auto TargetTriple = llvm::sys::getDefaultTargetTriple();
  module->setTargetTriple(TargetTriple);
}