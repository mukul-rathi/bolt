open Core
open Print_data_race_checker_ast

let%expect_test "Consume variable" =
  print_data_race_checker_ast
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;
      const int g : Bar ; 
      const int h : Bar;

    }
    class Choco {
       region thread Late;
      const int f : Late;
    }
    class Bana {
       region read Na;
      var int f : Na;
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
          └──Regions: Late
    └──Class: Bana
       └──Regions:
          └──Region: Read Na
       └──Field Defn: f
          └──Mode: Var
          └──Type expr: Int
          └──Regions: Na
    └──Main block
       └──Type expr: Int
       └──Expr: If
          └──Type expr: Int
          └──Expr: Bool:true
          └──Then block
             └──Type expr: Int
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
                      └──Regions:
                         └──Region: Linear Bar
                      └──Capability allowed?
                         └──Linear: true, Thread: true, Read: true, Subordinate: true, Locked: true
             └──Expr: Let var: _var_z0
                └──Type expr: Int
                └──Expr: Int:5
             └──Expr: Let var: _var_w0
                └──Type expr: Int
                └──Expr: Consume
                   └──Expr: Variable: _var_z0
                      └──Type expr: Int
             └──Expr: Objfield: (Class: Foo) _var_y0.h
                └──Type expr: Int
                └──Regions:
                   └──Region: Linear Bar
                └──Capability allowed?
                   └──Linear: true, Thread: true, Read: true, Subordinate: true, Locked: true
          └──Else block
             └──Type expr: Int
             └──Expr: If
                └──Type expr: Int
                └──Expr: Bool:false
                └──Then block
                   └──Type expr: Int
                   └──Expr: Let var: _var_x0
                      └──Type expr: Class: Choco
                      └──Expr: Constructor for: Choco
                         └──Type expr: Class: Choco
                         └── Field: f
                            └──Type expr: Int
                            └──Expr: Int:5
                   └──Expr: Let var: _var_y0
                      └──Type expr: Class: Choco
                      └──Expr: Consume
                         └──Expr: Variable: _var_x0
                            └──Type expr: Class: Choco
                            └──Regions:
                               └──Region: Thread Late
                            └──Capability allowed?
                               └──Linear: true, Thread: true, Read: true, Subordinate: true, Locked: true
                   └──Expr: Variable: _var_y0
                      └──Type expr: Class: Choco
                      └──Regions:
                         └──Region: Thread Late
                      └──Capability allowed?
                         └──Linear: true, Thread: true, Read: true, Subordinate: true, Locked: true
                   └──Expr: Int:1
                └──Else block
                   └──Type expr: Int
                   └──Expr: Let var: _var_x0
                      └──Type expr: Class: Bana
                      └──Expr: Constructor for: Bana
                         └──Type expr: Class: Bana
                         └── Field: f
                            └──Type expr: Int
                            └──Expr: Int:5
                   └──Expr: Let var: _var_y0
                      └──Type expr: Int
                      └──Expr: Consume
                         └──Expr: Objfield: (Class: Bana) _var_x0.f
                            └──Type expr: Int
                            └──Regions:
                               └──Region: Read Na
                            └──Capability allowed?
                               └──Linear: true, Thread: true, Read: true, Subordinate: true, Locked: true
                   └──Expr: Variable: _var_y0
                      └──Type expr: Int |}]
