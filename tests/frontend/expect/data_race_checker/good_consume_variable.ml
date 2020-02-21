open Core
open Print_data_race_checker_ast

let%expect_test "Consume linear variable" =
  print_data_race_checker_ast
    " 
    class Foo {
      region linear Bar;
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
       └──Regions:
          └──Region: Linear Bar
       └──Field Defn: f
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar
       └──Field Defn: g
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar
       └──Field Defn: h
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar
    └──Main block
       └──Type expr: Class: Foo
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:4
             └── Field: g
                └──Type expr: Int
                └──Expr: Int:5
             └── Field: h
                └──Type expr: Int
                └──Expr: Int:6
       └──Expr: Let var: _var_y0
          └──Type expr: Class: Foo
          └──Expr: Consume
             └──Expr: Variable: _var_x0
                └──Type expr: Class: Foo
                └── Possible Regions:
                   └── Possible Region: Linear Bar |}]

let%expect_test "Consume linear field of variable" =
  print_data_race_checker_ast
    " 
    class Foo {
      region thread Bar;
      var Baz f : Bar;
    }
   class Baz {
       region linear Fa;
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
       └──Regions:
          └──Region: Thread Bar
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Class: Baz
          └──Regions: Bar
    └──Class: Baz
       └──Regions:
          └──Region: Linear Fa
       └──Field Defn: g
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Fa
    └──Main block
       └──Type expr: Class: Baz
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Let var: _var_y0
          └──Type expr: Class: Baz
          └──Expr: Consume
             └──Expr: Objfield: (Class: Foo) _var_x0.f
                └──Type expr: Class: Baz
                └──Regions:
                   └──Region: Thread Bar |}]

let%expect_test "Consume linear variable" =
  print_data_race_checker_ast
    " 
    class Foo {
      region linear Bar, read Baz;
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
       └──Regions:
          └──Region: Linear Bar
          └──Region: Read Baz
       └──Field Defn: f
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar,Baz
       └──Field Defn: g
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar
       └──Field Defn: h
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar
    └──Main block
       └──Type expr: Class: Foo
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:4
             └── Field: g
                └──Type expr: Int
                └──Expr: Int:5
             └── Field: h
                └──Type expr: Int
                └──Expr: Int:6
       └──Expr: Let var: _var_z0
          └──Type expr: Class: Foo
          └──Expr: Variable: _var_x0
             └──Type expr: Class: Foo
             └── Possible Regions:
                └── Possible Region: Linear Bar
                └── Possible Region: Read Baz
       └──Expr: Objfield: (Class: Foo) _var_z0.f
          └──Type expr: Int
          └──Regions:
             └──Region: Read Baz
       └──Expr: Let var: _var_y0
          └──Type expr: Class: Foo
          └──Expr: Consume
             └──Expr: Variable: _var_x0
                └──Type expr: Class: Foo
                └── Possible Regions:
                   └── Possible Region: Linear Bar |}]
