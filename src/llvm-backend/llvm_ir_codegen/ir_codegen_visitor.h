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
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;

  std::map<std::string, llvm::AllocaInst *> varEnv;

  /* These methods are called when program code is generated */
  llvm::FunctionType *codegenFunctionType(const FunctionIR &function);
  void codegenFunctionArgs(const FunctionIR &function);
  void codegenFunction(const FunctionIR &function);
  void codegenClasses(const std::vector<std::unique_ptr<ClassIR>> &classes);
  void codegenFunctions(
      const std::vector<std::unique_ptr<FunctionIR>> &functions);
  void codegenMainExpr(const std::vector<std::unique_ptr<ExprIR>> &mainExpr);

  void codegenExternFunctionDeclarations();
  void codegenJoinPThreads(const std::vector<llvm::Value *> pthreads);
  void codegenCreatePThread(llvm::Value *pthread, const AsyncExprIR &asyncExpr);
  llvm::Function *codegenAsyncFunction(const AsyncExprIR &asyncExpr,
                                       llvm::StructType *functionArgType,
                                       llvm::Type *functionArgPointerType);
  llvm::Value *codegenAsyncFunctionArg(const AsyncExprIR &asyncExpr,
                                       llvm::StructType *functionArgType);

  void codegenPrintString(const std::string &str);

 public:
  IRCodegenVisitor();
  void configureTarget();
  void codegenProgram(const ProgramIR &program);
  void runOptimisingPasses();
  void dumpLLVMIR();
  std::string dumpLLVMIRToString();
  /* Codegen methods are public, since called by the IR object */
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
  virtual llvm::Value *codegen(const ExprFinishAsyncIR &expr) override;
  virtual llvm::Value *codegen(const ExprIfElseIR &expr) override;
  virtual llvm::Value *codegen(const ExprWhileLoopIR &expr) override;
  virtual llvm::Value *codegen(const ExprBinOpIR &expr) override;
  virtual llvm::Value *codegen(const ExprUnOpIR &expr) override;
  virtual llvm::Value *codegen(const ExprPrintfIR &expr) override;

  virtual llvm::Type *codegen(const TypeIntIR &typeIR) override;
  virtual llvm::Type *codegen(const TypeClassIR &typeIR) override;
  virtual llvm::Type *codegen(const TypeVoidIR &typeIR) override;
  virtual llvm::Type *codegen(const TypeBoolIR &typeIR) override;
};

class IRCodegenException : public std::exception {
  std::string errorMessage;

 public:
  IRCodegenException(const char msg[]) { std::string errorMessage(msg); }
  const char *what() const throw() { return errorMessage.c_str(); }
};
