open Core
open Print_data_race_checker_ast

let%expect_test "Consume linear variable" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
      const int f : Bar;
      const int g : Bar ; 
      const int h : Bar;
    }
    void main(){
        let x = new Foo(f:4, g:5, h:6);
        let y = consume x // Consume linear variable 
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Linear Bar
       └──Field Defn: f
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar
       └──Field Defn: g
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar
       └──Field Defn: h
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar
    └──Main block
       └──Type expr: Foo
       └──Expr: Let var: _x0
          └──Type expr: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:4
             └── Field: g
                └──Type expr: Int
                └──Expr: Int:5
             └── Field: h
                └──Type expr: Int
                └──Expr: Int:6
       └──Expr: Let var: _y0
          └──Type expr: Foo
          └──Expr: Consume
             └──Expr: Variable: _x0
                └──Type expr: Foo
                └── Possible Capabilities:
                   └── Possible Capability: Linear Bar |}]

let%expect_test "Consume linear field of variable" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability local Bar;
      var Baz f : Bar;
    }
   class Baz {
       capability linear Fa;
       var int g : Fa;
    }
    void main(){
        let x = new Foo();
        let y = consume x.f // Consume linear field of variable 

    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: ThreadLocal Bar
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Baz
          └──Capabilities: Bar
    └──Class: Baz
       └──Capabilities:
          └──Capability: Linear Fa
       └──Field Defn: g
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: Fa
    └──Main block
       └──Type expr: Baz
       └──Expr: Let var: _x0
          └──Type expr: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Foo
       └──Expr: Let var: _y0
          └──Type expr: Baz
          └──Expr: Consume
             └──Expr: Objfield: (Class: Foo) _x0.f
                └──Type expr: Baz
                └──Capabilities:
                   └──Capability: ThreadLocal Bar |}]

let%expect_test "Consume linear variable" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar, read Baz;
      const int f : Bar, Baz;
      const int g : Bar ; 
      const int h : Bar;
    }
    void main(){
        let x = new Foo(f:4, g:5, h:6);
        let z = x; 
        z.f; // z's liveness ends here
        let y = consume x // Consume linear variable 
      }

  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Linear Bar
          └──Capability: Read Baz
       └──Field Defn: f
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar,Baz
       └──Field Defn: g
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar
       └──Field Defn: h
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Bar
    └──Main block
       └──Type expr: Foo
       └──Expr: Let var: _x0
          └──Type expr: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:4
             └── Field: g
                └──Type expr: Int
                └──Expr: Int:5
             └── Field: h
                └──Type expr: Int
                └──Expr: Int:6
       └──Expr: Let var: _z0
          └──Type expr: Foo
          └──Expr: Variable: _x0
             └──Type expr: Foo
             └── Possible Capabilities:
                └── Possible Capability: Linear Bar
                └── Possible Capability: Read Baz
       └──Expr: Objfield: (Class: Foo) _z0.f
          └──Type expr: Int
          └──Capabilities:
             └──Capability: Read Baz
       └──Expr: Let var: _y0
          └──Type expr: Foo
          └──Expr: Consume
             └──Expr: Variable: _x0
                └──Type expr: Foo
                └── Possible Capabilities:
                   └── Possible Capability: Linear Bar |}]
