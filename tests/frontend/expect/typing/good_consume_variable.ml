open Core
open Print_typed_ast

let%expect_test "Consume variable" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;
      const int g : Bar ; 
      const int h : Bar;

    }
    class Choco {
       region thread Late;
      const int f : Bar;
    }
    class Bana {
       region read Na;
      const int f : Bar;
    }
    void main(){
      if true {
        let x = new Foo(f:4, g:5, h:6);
        let y = consume x; (* Consume linear variable *)
        let z = 5;
        let w = consume z; (* Can consume an int *)
        y.h
      }
      else {
        if false {
        let x = new Choco(f:5);
        let y = consume x;
        y.f
         }
       else{
         let x = new Bana(f:5);
         let y = consume x.f;
         y
         }
      }
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
          └──Regions: Bar
    └──Class: Bana
       └──Regions:
          └──Region: Read Na
       └──Field Defn: f
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar
    └──Expr: Block
       └──Type expr: Int
       └──Expr: If
          └──Type expr: Int
          └──Expr: Bool:true
          └──Expr: Block
             └──Type expr: Int
             └──Expr: Let var: x
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
             └──Expr: Let var: y
                └──Type expr: Class: Foo
                └──Expr: Consume
                   └──Expr: Variable: x
                      └──Type expr: Class: Foo
             └──Expr: Let var: z
                └──Type expr: Int
                └──Expr: Int:5
             └──Expr: Let var: w
                └──Type expr: Int
                └──Expr: Consume
                   └──Expr: Variable: z
                      └──Type expr: Int
             └──Expr: Objfield: (Class: Foo) y.h
                └──Type expr: Int
          └──Expr: Block
             └──Type expr: Int
             └──Expr: If
                └──Type expr: Int
                └──Expr: Bool:false
                └──Expr: Block
                   └──Type expr: Int
                   └──Expr: Let var: x
                      └──Type expr: Class: Choco
                      └──Expr: Constructor for: Choco
                         └──Type expr: Class: Choco
                         └── Field: f
                            └──Type expr: Int
                            └──Expr: Int:5
                   └──Expr: Let var: y
                      └──Type expr: Class: Choco
                      └──Expr: Consume
                         └──Expr: Variable: x
                            └──Type expr: Class: Choco
                   └──Expr: Objfield: (Class: Choco) y.f
                      └──Type expr: Int
                └──Expr: Block
                   └──Type expr: Int
                   └──Expr: Let var: x
                      └──Type expr: Class: Bana
                      └──Expr: Constructor for: Bana
                         └──Type expr: Class: Bana
                         └── Field: f
                            └──Type expr: Int
                            └──Expr: Int:5
                   └──Expr: Let var: y
                      └──Type expr: Int
                      └──Expr: Consume
                         └──Expr: Objfield: (Class: Bana) x.f
                            └──Type expr: Int
                   └──Expr: Variable: y
                      └──Type expr: Int |}]
