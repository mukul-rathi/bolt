open Core
open Print_parsed_ast

let%expect_test "Consume variable" =
  print_parsed_ast
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;
      const int g : Bar;
      const int h : Bar;
    }
    class Choco {
      region thread Late;
      const int f : Late;
    }
    class Bana {
      region read Na;
      const int f : Na;
    }
    void main(){
        let x1 = new Foo(f:4, g:5, h:6);
        let y1 = consume x1; // Consume linear variable 
        let x2 = new Choco(f:5);
        let y2 = consume x2.f; // Consume thread variable's field
        let x3 = new Bana(f:5);
        let y3 = consume x3 // Consume read variable 
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
    └──Class: Choco
       └──Regions:
          └──Region: Thread Late
       └──Field Defn: f
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Late
    └──Class: Bana
       └──Regions:
          └──Region: Read Na
       └──Field Defn: f
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Na
    └──Main block
       └──Expr: Let var: x1
          └──Expr: Constructor for: Foo
             └── Field: f
                └──Expr: Int:4
             └── Field: g
                └──Expr: Int:5
             └── Field: h
                └──Expr: Int:6
       └──Expr: Let var: y1
          └──Expr: Consume
             └──Expr: Variable: x1
       └──Expr: Let var: x2
          └──Expr: Constructor for: Choco
             └── Field: f
                └──Expr: Int:5
       └──Expr: Let var: y2
          └──Expr: Consume
             └──Expr: Objfield: x2.f
       └──Expr: Let var: x3
          └──Expr: Constructor for: Bana
             └── Field: f
                └──Expr: Int:5
       └──Expr: Let var: y3
          └──Expr: Consume
             └──Expr: Variable: x3 |}]
