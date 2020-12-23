
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

void IRCodegenVisitor::codegenExternFunctionDeclarations() {
  module->getOrInsertFunction(
      "printf", llvm::FunctionType::get(
                    llvm::IntegerType::getInt32Ty(*context),
                    llvm::PointerType::get(llvm::Type::getInt8Ty(*context), 0),
                    true /* this is var arg func type*/));

  // void * represented as i8*
  llvm::Type *voidPtrTy = llvm::Type::getInt8Ty(*context)->getPointerTo();

  // void *malloc(int64 size) - we use GC_malloc to get the BDW-GC for free!
  module->getOrInsertFunction(
      "GC_malloc", llvm::FunctionType::get(
                       voidPtrTy, llvm::IntegerType::getInt64Ty(*context),
                       /* has variadic args */ false));

  // PTHREADS

  llvm::Type *pthreadTy =
      llvm::StructType::create(*context, llvm::StringRef("pthread_t"));

  llvm::Type *pthreadPtrTy = pthreadTy->getPointerTo();

  // (void *) fn (void * arg)
  llvm::FunctionType *funVoidPtrVoidPtrTy = llvm::FunctionType::get(
      voidPtrTy, llvm::ArrayRef<llvm::Type *>({voidPtrTy}),
      /* has variadic args */ false);

  // int pthread_create(pthread_t ** thread, const pthread_attr_t * attr,
  //                  void * (*start_routine)(void *), void * arg)
  // we can use a void * in place of the opaque platform-specific
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
      llvm::ArrayRef<llvm::Type *>({pthreadPtrTy, pthreadPtrTy}),
      /* has variadic args */ false);
  module->getOrInsertFunction("pthread_equal", pthreadEqualTy);

  // pthread_t pthread_self ();
  llvm::FunctionType *pthreadSelfTy =
      llvm::FunctionType::get(pthreadPtrTy, llvm::ArrayRef<llvm::Type *>({}),
                              /* has variadic args */ false);
  module->getOrInsertFunction("pthread_self", pthreadSelfTy);
}