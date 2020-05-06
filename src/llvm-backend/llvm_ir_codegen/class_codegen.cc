#include <iostream>
#include <string>

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Type.h"
#include "src/llvm-backend/llvm_ir_codegen/ir_codegen_visitor.h"

llvm::Type *IRCodegenVisitor::codegen(const TypeIntIR &typeIR) {
  return llvm::Type::getInt32Ty(*context);
};
llvm::Type *IRCodegenVisitor::codegen(const TypeClassIR &typeIR) {
  return module->getTypeByName(llvm::StringRef(typeIR.className))
      ->getPointerTo();
  ;
};
llvm::Type *IRCodegenVisitor::codegen(const TypeVoidIR &typeIR) {
  return llvm::Type::getVoidTy(*context);
};
llvm::Type *IRCodegenVisitor::codegen(const TypeBoolIR &typeIR) {
  return llvm::Type::getInt1Ty(*context);  // bools are 1-bit integers
  ;
};

void IRCodegenVisitor::codegenClasses(
    const std::vector<std::unique_ptr<ClassIR>> &classes) {
  // create (opaque) struct types for each of the classes & their vtables
  for (auto &currClass : classes) {
    llvm::StructType::create(*context, llvm::StringRef(currClass->className));
    llvm::StructType::create(*context,
                             llvm::StringRef("_Vtable" + currClass->className));
  }
  // fill in struct bodies
  for (auto &currClass : classes) {
    llvm::StructType *classType =
        module->getTypeByName(llvm::StringRef(currClass->className));
    llvm::Type *pthreadPtrTy =
        module->getTypeByName(llvm::StringRef("pthread_t"))->getPointerTo();
    llvm::Type *vTablePtrTy =
        module->getTypeByName(llvm::StringRef("_Vtable" + currClass->className))
            ->getPointerTo();
    // first 4 fields are:
    // pointer to vTable
    // a pointer to the owning pthread
    // the counts of lock read and write owners respectively
    std::vector<llvm::Type *> bodyTypes({vTablePtrTy, pthreadPtrTy,
                                         llvm::Type::getInt32Ty(*context),
                                         llvm::Type::getInt32Ty(*context)});
    for (auto &field : currClass->fields) {
      bodyTypes.push_back(field->accept(*this));
    }
    classType->setBody(llvm::ArrayRef<llvm::Type *>(bodyTypes));
  }
}

void IRCodegenVisitor::codegenVTables(
    const std::vector<std::unique_ptr<ClassIR>> &classes) {
  // fill in struct bodies
  for (auto &currClass : classes) {
    std::string vTableName = "_Vtable" + currClass->className;
    llvm::StructType *vTableTy =
        module->getTypeByName(llvm::StringRef(vTableName));
    std::vector<llvm::Constant *> vTableMethods;
    std::vector<llvm::Type *> vTableMethodTys;
    for (auto &methodName : currClass->vtable) {
      llvm::Function *method = module->getFunction(llvm::StringRef(methodName));
      vTableMethods.push_back(method);
      vTableMethodTys.push_back(method->getType());
    }
    vTableTy->setBody(vTableMethodTys);
    module->getOrInsertGlobal(vTableName, vTableTy);
    llvm::GlobalVariable *vTable = module->getNamedGlobal(vTableName);
    vTable->setLinkage(llvm::GlobalValue::CommonLinkage);
    vTable->setInitializer(llvm::ConstantStruct::get(vTableTy, vTableMethods));
  }
}
