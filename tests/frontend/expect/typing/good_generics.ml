open Core
open Print_typed_ast

let%expect_test "Generic field assigned different types depending on instantiation" =
  print_typed_ast
    " 
    class Foo<T> {
      capability linear Bar;
      var T f : Bar;
    }
    void main() {
      let x = new Foo<int>(f:100);
      let y = new Foo<bool>(f:true)

    }
  " ;
  [%expect
    {|
      Program
      └──Class: Foo<T>
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: T
            └──Capabilities: Bar
      └──Main block
         └──Type expr: Foo<Bool>
         └──Expr: Let var: x
            └──Type expr: Foo<Int>
            └──Expr: Constructor for: Foo
               └──Type expr: Foo<Int>
               └── Field: f
                  └──Type expr: Int
                  └──Expr: Int:100
         └──Expr: Let var: y
            └──Type expr: Foo<Bool>
            └──Expr: Constructor for: Foo
               └──Type expr: Foo<Bool>
               └── Field: f
                  └──Type expr: Bool
                  └──Expr: Bool:true |}]

let%expect_test "Method takes in generic type of same type as argument" =
  print_typed_ast
    " 
    class Foo<T>{
      capability linear Bar;
      var T f : Bar;
      void copy(Foo<T> x): Bar{
        this.f := x.f
      }
    }
    void main() {
      let x =  new Foo<int>(f:5);
      let y =  new Foo<int>();
      y.copy(consume x)
    }
  " ;
  [%expect
    {|
      Program
      └──Class: Foo<T>
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: T
            └──Capabilities: Bar
         └── Method: copy
            └── Return type: Void
            └──Param: x
               └──Type expr: Foo<T>
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: T
               └──Expr: Assign
                  └──Type expr: T
                  └──Expr: Objfield: (Class: Foo<T>) this.f
                     └──Type expr: T
                  └──Expr: Objfield: (Class: Foo<T>) x.f
                     └──Type expr: T
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: x
            └──Type expr: Foo<Int>
            └──Expr: Constructor for: Foo
               └──Type expr: Foo<Int>
               └── Field: f
                  └──Type expr: Int
                  └──Expr: Int:5
         └──Expr: Let var: y
            └──Type expr: Foo<Int>
            └──Expr: Constructor for: Foo
               └──Type expr: Foo<Int>
         └──Expr: ObjMethod: (Class: Foo<Int>) y.copy
            └──Type expr: Int
            └──Expr: Consume
               └──Expr: Variable: x
                  └──Type expr: Foo<Int> |}]
