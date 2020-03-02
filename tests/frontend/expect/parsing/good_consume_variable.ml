open Core
open Print_parsed_ast

let%expect_test "Consume variable" =
  print_parsed_ast
    " 
    class Foo {
      capability linear Bar;
      const int f : Bar;
      const int g : Bar;
      const int h : Bar;
    }
    class Choco {
      capability local Late;
      const int f : Late;
    }
    class Bana {
      capability read Na;
      const int f : Na;
    }
    void main(){
        let x1 = new Foo(f:4, g:5, h:6);
        let y1 = consume x1; // Consume linear variable 
        let x2 = new Choco(f:5);
        let y2 = consume x2.f; // Consume local variable's field
        let x3 = new Bana(f:5);
        let y3 = consume x3 // Consume read variable 
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
    └──Class: Choco
       └──Capabilities:
          └──Capability: ThreadLocal Late
       └──Field Defn: f
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Late
    └──Class: Bana
       └──Capabilities:
          └──Capability: Read Na
       └──Field Defn: f
          └──Modifier: Const
          └──Type expr: Int
          └──Capabilities: Na
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
