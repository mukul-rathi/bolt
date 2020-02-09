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
    expr->accept(*this);
  }

  llvm::APInt retVal(32 /* bitSize */, (uint32_t)0, true /* signed */);
  builder->CreateRet(llvm::ConstantInt::get(*(context), retVal));
  llvm::verifyFunction(*main);
}

void IRCodegenVisitor::codegenExternFunctionDeclarations() {
  module->getOrInsertFunction(
      "printf", llvm::FunctionType::get(
                    llvm::IntegerType::getInt32Ty(*context),
                    llvm::PointerType::get(llvm::Type::getInt8Ty(*context), 0),
                    true /* this is var arg func type*/));

  // void * represented as i8*
  llvm::Type *voidPtrTy = llvm::Type::getInt8Ty(*context)->getPointerTo();

  // void *malloc(int64 size)
  module->getOrInsertFunction(
      "malloc", llvm::FunctionType::get(voidPtrTy,
                                        llvm::IntegerType::getInt64Ty(*context),
                                        /* has variadic args */ false));

  // PTHREADS

  llvm::Type *pthreadTy =
      llvm::StructType::create(*context, llvm::StringRef("pthread_t"));

  llvm::Type *pthreadPtrTy = pthreadTy->getPointerTo();

  // (void *) fn (void * arg)
  llvm::FunctionType *funVoidPtrVoidPtrTy = llvm::FunctionType::get(
      voidPtrTy, llvm::ArrayRef<llvm::Type *>({voidPtrTy}),
      /* has variadic args */ false);

  // int pthread_create(pthread_t ** thread, const pthread_attr_t * attr, \
  //                  void * (*start_routine)(void *), void * arg)
  // we can use a void * in place of the opaque platform-specific pthread_t *,
  // pthread_attr_t *
  llvm::FunctionType *pthreadCreateTy = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(*context),
      llvm::ArrayRef<llvm::Type *>({pthreadPtrTy->getPointerTo(), voidPtrTy,
                                    (funVoidPtrVoidPtrTy)->getPointerTo(),
                                    voidPtrTy}),
      /* has variadic args */ false);
  module->getOrInsertFunction("pthread_create", pthreadCreateTy);

  // int pthread_join(pthread_t thread, void **value_ptr)
  llvm::FunctionType *pthreadJoinTy = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(*context),
      llvm::ArrayRef<llvm::Type *>({pthreadPtrTy, voidPtrTy->getPointerTo()}),
      /* has variadic args */ false);
  module->getOrInsertFunction("pthread_join", pthreadJoinTy);

  // int pthread_equal (pthread_t t1, pthread_t t2);
  llvm::FunctionType *pthreadEqualTy = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(*context),
      llvm::ArrayRef<llvm::Type *>({pthreadTy, pthreadTy}),
      /* has variadic args */ false);
  module->getOrInsertFunction("pthread_equal", pthreadEqualTy);

  // pthread_t pthread_self ();
  llvm::FunctionType *pthreadSelfTy =
      llvm::FunctionType::get(pthreadTy, llvm::ArrayRef<llvm::Type *>({}),
                              /* has variadic args */ false);
  module->getOrInsertFunction("pthread_self", pthreadSelfTy);
}
void IRCodegenVisitor::codegenProgram(const ProgramIR &program) {
  codegenExternFunctionDeclarations();
  codegenClasses(program.classDefns);
  codegenFunctions(program.functionDefns);
  codegenMainExpr(program.mainExpr);
}

void IRCodegenVisitor::configureTarget() {
  auto TargetTriple = llvm::sys::getDefaultTargetTriple();
  module->setTargetTriple(TargetTriple);
}

void IRCodegenVisitor::runOptimisingPasses() {
  std::unique_ptr<llvm::legacy::FunctionPassManager> functionPassManager =
      llvm::make_unique<llvm::legacy::FunctionPassManager>(module.get());
  functionPassManager->add(llvm::createPromoteMemoryToRegisterPass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  functionPassManager->add(llvm::createInstructionCombiningPass());
  // Reassociate expressions.
  functionPassManager->add(llvm::createReassociatePass());
  // Eliminate Common SubExpressions.
  functionPassManager->add(llvm::createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  functionPassManager->add(llvm::createCFGSimplificationPass());

  functionPassManager->doInitialization();
}