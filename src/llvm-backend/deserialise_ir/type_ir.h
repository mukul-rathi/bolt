#pragma once
#include <string>
#include <vector>
struct TypeIR {
  virtual ~TypeIR() = default;
};

struct TypeIntIR : public TypeIR {};

struct TypeClassIR : public TypeIR {
  std::string className;
};

struct TypeVoidIR : public TypeIR {};

struct TypeBoolIR : public TypeIR {};