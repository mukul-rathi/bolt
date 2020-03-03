open Core
open Print_data_race_checker_ast

let%expect_test "Use variable without needing any capabilities" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
      int id (int x): Bar { x}
    }
    void main(){
      let x = new Foo(); 
      let y = x; 
      y // fine to not have capabilities since we're not using any. 
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Linear Bar
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: Bar
       └── Method: id
          └── Return type: Int
          └──Param: x
             └──Type expr: Int
          └── Used capabilities
          └──   Capabilities: Bar
          └──Body block
             └──Type expr: Int
             └──Expr: Variable: x
                └──Type expr: Int
    └──Main block
       └──Type expr: Class: Foo
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Let var: _var_y0
          └──Type expr: Class: Foo
          └──Expr: Variable: _var_x0
             └──Type expr: Class: Foo
             └── Possible Capabilities:
                └── Possible Capability: Linear Bar
       └──Expr: Variable: _var_y0
          └──Type expr: Class: Foo
          └── Possible Capabilities: |}]

let%expect_test "Simple linear class" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
      int id (int x): Bar { x}
    }
    void main(){
      let x = new Foo(); 
      x.f:= x.id(5)

    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Linear Bar
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: Bar
       └── Method: id
          └── Return type: Int
          └──Param: x
             └──Type expr: Int
          └── Used capabilities
          └──   Capabilities: Bar
          └──Body block
             └──Type expr: Int
             └──Expr: Variable: x
                └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Assign
          └──Type expr: Int
          └──Expr: Objfield: (Class: Foo) _var_x0.f
             └──Type expr: Int
             └──Capabilities:
                └──Capability: Linear Bar
          └──Expr: ObjMethod: (Class: Foo) _var_x0.id
             └── Possible Capabilities:
                └── Possible Capability: Linear Bar
             └──Type expr: Int
             └──Expr: Int:5 |}]

let%expect_test "Simple local class" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability local Bar;
      var int f : Bar;
    }
   void main(){
      let x = new Foo(); 
      x.f:= 5
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
          └──Type expr: Int
          └──Capabilities: Bar
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Assign
          └──Type expr: Int
          └──Expr: Objfield: (Class: Foo) _var_x0.f
             └──Type expr: Int
             └──Capabilities:
                └──Capability: ThreadLocal Bar
          └──Expr: Int:5 |}]

let%expect_test "Simple read class" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability read Bar;
      const bool f : Bar;
    }
    void main(){
      let x = new Foo(f:true); 
      x.f
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Read Bar
       └──Field Defn: f
          └──Modifier: Const
          └──Type expr: Bool
          └──Capabilities: Bar
    └──Main block
       └──Type expr: Bool
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
             └── Field: f
                └──Type expr: Bool
                └──Expr: Bool:true
       └──Expr: Objfield: (Class: Foo) _var_x0.f
          └──Type expr: Bool
          └──Capabilities:
             └──Capability: Read Bar |}]
