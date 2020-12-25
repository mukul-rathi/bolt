
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

llvm::Type *IRCodegenVisitor::codegenPthreadTy() {
  // a pthread is represented as some opaque struct *
  // but all pointers have the same width, so return void*

  return llvm::Type::getInt8Ty(*context)->getPointerTo();
}

void IRCodegenVisitor::codegenExternFunctionDeclarations() {
  // int printf ( const char * format, ... );
  module->getOrInsertFunction(
      "printf",
      llvm::FunctionType::get(llvm::IntegerType::getInt32Ty(*context),
                              llvm::Type::getInt8Ty(*context)->getPointerTo(),
                              true /* this is var arg func type*/));

  // void * represented as i8*
  llvm::Type *voidPtrTy = llvm::Type::getInt8Ty(*context)->getPointerTo();

  // void *malloc(int64 size)
  // GC_malloc has the same signature: we use GC_malloc to get the BDW-GC for
  // free!
  module->getOrInsertFunction(
      "GC_malloc", llvm::FunctionType::get(
                       voidPtrTy, llvm::IntegerType::getInt64Ty(*context),
                       /* has variadic args */ false));

  // PTHREADS

  llvm::Type *pthreadTy = codegenPthreadTy();

  llvm::Type *pthreadPtrTy = pthreadTy->getPointerTo();

  // (void *) fn (void * arg)
  llvm::FunctionType *funVoidPtrVoidPtrTy = llvm::FunctionType::get(
      voidPtrTy, llvm::ArrayRef<llvm::Type *>({voidPtrTy}),
      /* has variadic args */ false);

  // int pthread_create(pthread_t * thread, const pthread_attr_t * attr,
  //                  void * (*start_routine)(void *), void * arg)
  // we can use a void * in place of the opaque platform-specific
  // pthread_attr_t *
  llvm::FunctionType *pthreadCreateTy = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(*context),
      llvm::ArrayRef<llvm::Type *>({pthreadPtrTy, voidPtrTy,
                                    (funVoidPtrVoidPtrTy)->getPointerTo(),
                                    voidPtrTy}),
      /* has variadic args */ false);
  module->getOrInsertFunction("pthread_create", pthreadCreateTy);

  // int pthread_join(pthread_t thread, void **value_ptr)
  llvm::FunctionType *pthreadJoinTy = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(*context),
      llvm::ArrayRef<llvm::Type *>({pthreadTy, voidPtrTy->getPointerTo()}),
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