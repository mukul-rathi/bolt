
#include <string>

#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/ThreadPool.h"
#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

// we implement a re-entrant reader-writer lock

llvm::Value *IRCodegenVisitor::codegen(const ExprLockIR &lockExpr) {
  llvm::AllocaInst *objPtr = varEnv[lockExpr.objName];
  llvm::Value *obj = builder->CreateLoad(objPtr);  // get heap ptr
  llvm::Type *objType =
      objPtr->getAllocatedType()
          ->getPointerElementType();  // get type of element on heap
  llvm::Value *objOwnerThreadPtr = builder->CreateStructGEP(objType, obj, 1);
  llvm::Value *readLockCounterPtr =
      builder->CreateStructGEP(objType, obj, LockType::Reader);
  llvm::Value *writeLockCounterPtr =
      builder->CreateStructGEP(objType, obj, LockType::Writer);
  llvm::Function *pthread_self =
      module->getFunction(llvm::StringRef("pthread_self"));
  llvm::Value *currentThread = builder->CreateCall(pthread_self, {});

  // define basic blocks
  llvm::Function *parentFunction = builder->GetInsertBlock()->getParent();
  // check if we can acquire lock
  llvm::BasicBlock *spinOnLockFreeBB =
      llvm::BasicBlock::Create(*context, "spinOnLockFree", parentFunction);
  // try to acquire lock and increment counter
  llvm::BasicBlock *attemptlockIncBB =
      llvm::BasicBlock::Create(*context, "attemptlockInc", parentFunction);
  // lock successfully acquired
  llvm::BasicBlock *enterLockBB =
      llvm::BasicBlock::Create(*context, "enterLock", parentFunction);

  builder->CreateBr(spinOnLockFreeBB);

  // check if lock free
  builder->SetInsertPoint(spinOnLockFreeBB);
  llvm::Value *currReadLockCounter = builder->CreateLoad(readLockCounterPtr);
  llvm::Value *currWriteLockCounter = builder->CreateLoad(writeLockCounterPtr);
  llvm::Value *currObjOwnerThread = builder->CreateLoad(objOwnerThreadPtr);

  // check if there are other writers on other threads.
  llvm::Value *noWritersPresent = builder->CreateICmpEQ(
      currWriteLockCounter,
      llvm::ConstantInt::getSigned((llvm::Type::getInt32Ty(*context)), 0),
      "noWritersPresent");
  llvm::Function *pthread_equal =
      module->getFunction(llvm::StringRef("pthread_equal"));
  llvm::Value *currThreadOwnsLock = builder->CreateICmpNE(
      /* pthread_equal returns 0 if false, non-zero value if true */
      builder->CreateCall(pthread_equal, {currObjOwnerThread, currentThread}),
      llvm::ConstantInt::getSigned((llvm::Type::getInt32Ty(*context)), 0),
      "currThreadOwnsWriteLock");
  llvm::Value *noWritersPresentOnOtherThreads = builder->CreateOr(
      noWritersPresent, currThreadOwnsLock, "noWritersPresentOnOtherThreads");

  llvm::Value *canAcquireLock;
  if (lockExpr.lockType == LockType::Reader) {
    canAcquireLock = noWritersPresentOnOtherThreads;

  } else {  // lock type is writer
    llvm::Value *noReadersPresent = builder->CreateICmpEQ(
        currReadLockCounter,
        llvm::ConstantInt::getSigned((llvm::Type::getInt32Ty(*context)), 0),
        "noReadersPresent");
    canAcquireLock =
        builder->CreateAnd(noReadersPresent, noWritersPresentOnOtherThreads,
                           "canAcquireWriteLock");
  }

  builder->CreateCondBr(canAcquireLock, attemptlockIncBB, spinOnLockFreeBB);

  // try to increment lock
  builder->SetInsertPoint(attemptlockIncBB);

  llvm::Value *counterPtrToInc;
  llvm::Value *currLockCounterVal;
  if (lockExpr.lockType == LockType::Reader) {
    counterPtrToInc = readLockCounterPtr;
    currLockCounterVal = currReadLockCounter;
  } else {
    counterPtrToInc = writeLockCounterPtr;
    currLockCounterVal = currWriteLockCounter;
  }
  llvm::Value *lockCounterValAfterInc = builder->CreateAdd(
      currLockCounterVal,
      llvm::ConstantInt::getSigned((llvm::Type::getInt32Ty(*context)), 1),
      "inc");
  llvm::Value *cmpXChgStruct = builder->CreateAtomicCmpXchg(
      counterPtrToInc, currLockCounterVal, lockCounterValAfterInc,
      /* success ordering */
      llvm::AtomicOrdering::SequentiallyConsistent,
      /* failure ordering */ llvm::AtomicOrdering::Monotonic);

  // cmpxchg returns a struct - first value = original value, second value
  // is the 1 bit success condition. So we allocate it on the stack and get a
  // pointer to it, so we can access the second field using GEP
  llvm::AllocaInst *cmpXChgStructPtr =
      builder->CreateAlloca(cmpXChgStruct->getType(), nullptr);
  builder->CreateStore(cmpXChgStruct, cmpXChgStructPtr);
  llvm::Value *incSuccessful = builder->CreateLoad(builder->CreateStructGEP(
      cmpXChgStructPtr->getAllocatedType(), cmpXChgStructPtr, 1));
  builder->CreateCondBr(incSuccessful, enterLockBB, spinOnLockFreeBB);

  // if we've entered the lock, set the current owner of the lock to this
  // thread
  builder->SetInsertPoint(enterLockBB);
  return builder->CreateStore(currentThread, objOwnerThreadPtr);
}

/*


UNLOCK


*/

llvm::Value *IRCodegenVisitor::codegen(const ExprUnlockIR &unlockExpr) {
  llvm::AllocaInst *objPtr = varEnv[unlockExpr.objName];
  llvm::Value *obj = builder->CreateLoad(objPtr);
  llvm::Value *lockCounterPtr = builder->CreateStructGEP(
      objPtr->getAllocatedType()
          ->getPointerElementType() /* get type of element on heap*/,
      obj /*get heap ptr */, unlockExpr.lockType);

  llvm::Value *currLockCounter = builder->CreateLoad(lockCounterPtr);
  // if counter > 1 then counter-1 > 0 so still locked. If not, then we're
  // unlocking the object as counter=0, so should flush all load / stores.
  llvm::Value *shouldDecWithoutMemFence = builder->CreateICmpSGT(
      currLockCounter,
      llvm::ConstantInt::getSigned((llvm::Type::getInt32Ty(*context)), 1),
      "gt");

  //  create basic blocks
  llvm::Function *parentFunction = builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *memFenceBB =
      llvm::BasicBlock::Create(*context, "unlockMemFence", parentFunction);
  llvm::BasicBlock *unlockDecBB =
      llvm::BasicBlock::Create(*context, "unlockDec", parentFunction);

  builder->CreateCondBr(shouldDecWithoutMemFence, unlockDecBB, memFenceBB);

  builder->SetInsertPoint(memFenceBB);
  builder->CreateFence(llvm::AtomicOrdering::SequentiallyConsistent);
  builder->CreateBr(unlockDecBB);

  builder->SetInsertPoint(unlockDecBB);

  llvm::Value *decLockCounter = builder->CreateSub(
      currLockCounter,
      llvm::ConstantInt::getSigned((llvm::Type::getInt32Ty(*context)), 1),
      "dec");
  return builder->CreateStore(decLockCounter, lockCounterPtr);
}