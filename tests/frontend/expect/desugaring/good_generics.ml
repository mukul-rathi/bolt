open Core
open Print_desugared_ast

let%expect_test "Generic class that is uninstantiated is dropped" =
  print_desugared_ast
    " 
    class Foo<T> {
      capability linear Bar;
      var T f : Bar;
    }
    void main() {
    }
  " ;
  [%expect
    {|
      Program
      └──Main block
         └──Type expr: Void |}]

let%expect_test "Generic field assigned different types depending on instantiation" =
  print_desugared_ast
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
      └──Class: _FooBool
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Bool
            └──Capabilities: Bar
      └──Class: _FooInt
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Bar
      └──Main block
         └──Type expr: _FooBool
         └──Expr: Let var: _var_x0
            └──Type expr: _FooInt
            └──Expr: Constructor for: _FooInt
               └──Type expr: _FooInt
               └── Field: f
                  └──Type expr: Int
                  └──Expr: Int:100
         └──Expr: Let var: _var_y0
            └──Type expr: _FooBool
            └──Expr: Constructor for: _FooBool
               └──Type expr: _FooBool
               └── Field: f
                  └──Type expr: Bool
                  └──Expr: Bool:true |}]

let%expect_test "Method takes in generic type of same type as argument" =
  print_desugared_ast
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
      └──Class: _FooInt
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Bar
         └── Method: copy
            └── Return type: Void
            └──Param: x
               └──Type expr: _FooInt
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Assign
                  └──Type expr: Int
                  └──Expr: Objfield: (Class: _FooInt) this.f
                     └──Type expr: Int
                     └──Capabilities:
                        └──Capability: Linear Bar
                  └──Expr: Objfield: (Class: _FooInt) x.f
                     └──Type expr: Int
                     └──Capabilities:
                        └──Capability: Linear Bar
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: _var_x0
            └──Type expr: _FooInt
            └──Expr: Constructor for: _FooInt
               └──Type expr: _FooInt
               └── Field: f
                  └──Type expr: Int
                  └──Expr: Int:5
         └──Expr: Let var: _var_y0
            └──Type expr: _FooInt
            └──Expr: Constructor for: _FooInt
               └──Type expr: _FooInt
         └──Expr: ObjMethod: (Class: _FooInt) _var_y0.copy
            └── Possible Capabilities:
               └── Possible Capability: Linear Bar
            └──Type expr: Int
            └──Expr: Consume
               └──Expr: Variable: _var_x0
                  └──Type expr: _FooInt
                  └── Possible Capabilities:
                     └── Possible Capability: Linear Bar |}]
