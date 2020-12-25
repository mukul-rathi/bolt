#pragma once
#include <stdlib.h>

#include <string>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "src/llvm-backend/deserialise_ir/class_ir.h"
#include "src/llvm-backend/deserialise_ir/expr_ir.h"
#include "src/llvm-backend/deserialise_ir/function_ir.h"
#include "src/llvm-backend/deserialise_ir/ir_visitor.h"
#include "src/llvm-backend/deserialise_ir/program_ir.h"
#include "src/llvm-backend/deserialise_ir/type_ir.h"

class IRCodegenVisitor : public IRVisitor {
  const int NUM_RESERVED_FIELDS = 4;

 protected: /* Used by tester */
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;

  std::map<std::string, llvm::AllocaInst *> varEnv;

 public:
  IRCodegenVisitor();

  // Top level methods (in ir_codegen_visitor.cc)
  void configureTarget();
  void runOptimisingPasses(
      const std::vector<std::unique_ptr<FunctionIR>> &functions);
  void dumpLLVMIR();
  std::string dumpLLVMIRToString();
  void codegenProgram(const ProgramIR &program);
  void codegenMainExpr(const std::vector<std::unique_ptr<ExprIR>> &mainExpr);

  // codegen for external function declarations (in extern_functions_codegen.cc)
  llvm::Type *codegenPthreadTy();
  void codegenExternFunctionDeclarations();

  // codegen for class definitions (in class_codegen.cc)
  void codegenClasses(const std::vector<std::unique_ptr<ClassIR>> &classes);
  void codegenVTables(const std::vector<std::unique_ptr<ClassIR>> &classes);

  // codegen for function definitions (in function_codegen.cc)
  llvm::FunctionType *codegenFunctionType(const FunctionIR &function);
  void codegenFunctionProtos(
      const std::vector<std::unique_ptr<FunctionIR>> &functions);
  void codegenFunctionDefn(const FunctionIR &function);
  void codegenFunctionDefns(
      const std::vector<std::unique_ptr<FunctionIR>> &functions);

  // codegen for pthread concurrency (in pthread_codegen.cc)
  void codegenJoinPThreads(const std::vector<llvm::Value *> pthreads);
  void codegenCreatePThread(llvm::Value *pthread, const AsyncExprIR &asyncExpr);
  llvm::Function *codegenAsyncFunction(const AsyncExprIR &asyncExpr,
                                       llvm::StructType *functionArgType,
                                       llvm::Type *functionArgPointerType);
  llvm::Value *codegenAsyncFunctionArg(const AsyncExprIR &asyncExpr,
                                       llvm::StructType *functionArgType);

  // Codegen methods for each of the expressions (in expr_codegen.cc)
  virtual llvm::Value *codegen(const IdentifierVarIR &var) override;
  virtual llvm::Value *codegen(const IdentifierObjFieldIR &objField) override;
  virtual llvm::Value *codegen(const ExprIntegerIR &expr) override;
  virtual llvm::Value *codegen(const ExprBooleanIR &expr) override;
  virtual llvm::Value *codegen(const ExprIdentifierIR &expr) override;
  virtual llvm::Value *codegen(const ExprConstructorIR &expr) override;
  virtual llvm::Value *codegen(const ExprLetIR &expr) override;
  virtual llvm::Value *codegen(const ExprAssignIR &expr) override;
  virtual llvm::Value *codegen(const ExprConsumeIR &expr) override;
  virtual llvm::Value *codegen(const ExprFunctionAppIR &expr) override;
  virtual llvm::Value *codegen(const ExprMethodAppIR &expr) override;
  virtual llvm::Value *codegen(const ExprFinishAsyncIR &expr) override;
  virtual llvm::Value *codegen(const ExprIfElseIR &expr) override;
  virtual llvm::Value *codegen(const ExprWhileLoopIR &expr) override;
  virtual llvm::Value *codegen(const ExprBinOpIR &expr) override;
  virtual llvm::Value *codegen(const ExprUnOpIR &expr) override;
  virtual llvm::Value *codegen(const ExprPrintfIR &expr) override;
  virtual llvm::Value *codegen(const ExprBlockIR &expr) override;
  virtual llvm::Value *codegen(const ExprLockIR &expr) override;
  virtual llvm::Value *codegen(const ExprUnlockIR &expr) override;

  // codegen for each of the types
  virtual llvm::Type *codegen(const TypeIntIR &typeIR) override;
  virtual llvm::Type *codegen(const TypeClassIR &typeIR) override;
  virtual llvm::Type *codegen(const TypeVoidIR &typeIR) override;
  virtual llvm::Type *codegen(const TypeBoolIR &typeIR) override;
};

// custom exception for code generation
class IRCodegenException : public std::exception {
  std::string errorMessage;

 public:
  IRCodegenException(std::string msg)
      : errorMessage("IR Codegen Error: " + msg){};
  const char *what() const throw() { return errorMessage.c_str(); }
};
