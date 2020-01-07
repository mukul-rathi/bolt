#pragma once
#include <string>
#include <vector>
class TypeIR {
 public:
  virtual void codegen();
};

class TypeIntIR : public TypeIR {};

class TypeClassIR : public TypeIR {
  std::string className;

 public:
  void setString(std::string str);
};

class TypeVoidIR : public TypeIR {};

class TypeBoolIR : public TypeIR {};