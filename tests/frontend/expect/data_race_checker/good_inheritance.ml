open Core
open Print_data_race_checker_ast

let%expect_test "Simple class inheritance" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
    }
    void main() {
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
      └──Class: Baz extends Foo
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
      └──Main block
         └──Type expr: Void |}]

let%expect_test "Access field of superclass" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
    }
    void main() {
      let x = new Baz();
      x.f
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
      └──Class: Baz extends Foo
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: _var_x0
            └──Type expr: Baz
            └──Expr: Constructor for: Baz
               └──Type expr: Baz
         └──Expr: Objfield: (Class: Baz) _var_x0.f
            └──Type expr: Int
            └──Capabilities:
               └──Capability: Linear Bar |}]

let%expect_test "Access field of super-superclass" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
    }
    class Banana extends Baz{
      capability read Haha;
      var int h : Haha;
    }
    void main() {
      let x = new Banana();
      x.f
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
      └──Class: Baz extends Foo
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
      └──Class: Banana extends Baz
         └──Capabilities:
            └──Capability: Read Haha
         └──Field Defn: h
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Haha
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: _var_x0
            └──Type expr: Banana
            └──Expr: Constructor for: Banana
               └──Type expr: Banana
         └──Expr: Objfield: (Class: Banana) _var_x0.f
            └──Type expr: Int
            └──Capabilities:
               └──Capability: Linear Bar |}]

let%expect_test "Access method of superclass" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;

      int getF() : Bar{
        this.f
      }
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
    }
    void main() {
      let x = new Baz();
      x.getF()
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
         └── Method: _getF
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Foo) this.f
                  └──Type expr: Int
                  └──Capabilities:
                     └──Capability: Linear Bar
      └──Class: Baz extends Foo
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: _var_x0
            └──Type expr: Baz
            └──Expr: Constructor for: Baz
               └──Type expr: Baz
         └──Expr: ObjMethod: (Class: Baz) _var_x0._getF
            └── Possible Capabilities:
               └── Possible Capability: Linear Bar
               └── Possible Capability: Linear Boo
            └──Type expr: Int
            └──() |}]

let%expect_test "Override method of superclass" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;

      int get() : Bar{
        this.f
      }
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
      int get() : Boo{
        this.g
      }

    }
    void main() {
      let x = new Baz();
      x.get()
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
         └── Method: _get
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Foo) this.f
                  └──Type expr: Int
                  └──Capabilities:
                     └──Capability: Linear Bar
      └──Class: Baz extends Foo
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
         └── Method: _get
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Boo
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Baz) this.g
                  └──Type expr: Int
                  └──Capabilities:
                     └──Capability: Linear Boo
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: _var_x0
            └──Type expr: Baz
            └──Expr: Constructor for: Baz
               └──Type expr: Baz
         └──Expr: ObjMethod: (Class: Baz) _var_x0._get
            └── Possible Capabilities:
               └── Possible Capability: Linear Bar
               └── Possible Capability: Linear Boo
            └──Type expr: Int
            └──() |}]

let%expect_test "Overload method of superclass" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;

      int get() : Bar{
        this.f
      }
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
      void get(int f) : Boo{
        this.g
      }

    }
    void main() {
      let x = new Baz();
      x.get(1)
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
         └── Method: _get
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Foo) this.f
                  └──Type expr: Int
                  └──Capabilities:
                     └──Capability: Linear Bar
      └──Class: Baz extends Foo
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
         └── Method: _geti
            └── Return type: Void
            └──Param: f
               └──Type expr: Int
            └── Used capabilities
            └──   Capabilities: Boo
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Baz) this.g
                  └──Type expr: Int
                  └──Capabilities:
                     └──Capability: Linear Boo
      └──Main block
         └──Type expr: Void
         └──Expr: Let var: _var_x0
            └──Type expr: Baz
            └──Expr: Constructor for: Baz
               └──Type expr: Baz
         └──Expr: ObjMethod: (Class: Baz) _var_x0._geti
            └── Possible Capabilities:
               └── Possible Capability: Linear Bar
               └── Possible Capability: Linear Boo
            └──Type expr: Void
            └──Expr: Int:1 |}]

let%expect_test "Pass in subtype to function" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
    }
    class Banana extends Baz{
      capability read Haha;
      var int h : Haha;
    }
    function int test(borrowed Foo x){
       x.f
    }
    void main() {
      let x = new Banana();
      test(x)
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
      └──Class: Baz extends Foo
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
      └──Class: Banana extends Baz
         └──Capabilities:
            └──Capability: Read Haha
         └──Field Defn: h
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Haha
      └── Function: test
         └── Return type: Int
         └──Borrowed Param: x
            └──Type expr: Foo
         └──Body block
            └──Type expr: Int
            └──Expr: Borrowed Objfield: (Class: Foo) x.f
               └──Type expr: Int
               └──Capabilities:
                  └──Capability: Linear Bar
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: _var_x0
            └──Type expr: Banana
            └──Expr: Constructor for: Banana
               └──Type expr: Banana
         └──Expr: Function App
            └──Type expr: Int
            └──Function: test
            └──Expr: Variable: _var_x0
               └──Type expr: Banana
               └── Possible Capabilities:
                  └── Possible Capability: Linear Bar
                  └── Possible Capability: Linear Boo
                  └── Possible Capability: Read Haha |}]

let%expect_test "Return subtype from function" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
    }
    class Banana extends Baz{
      capability read Haha;
      var int h : Haha;
    }
    function Foo test() {
        new Banana()
    }
    void main() {
      test()
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
      └──Class: Baz extends Foo
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
      └──Class: Banana extends Baz
         └──Capabilities:
            └──Capability: Read Haha
         └──Field Defn: h
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Haha
      └── Function: test
         └── Return type: Foo
         └──Param: Void
         └──Body block
            └──Type expr: Banana
            └──Expr: Constructor for: Banana
               └──Type expr: Banana
      └──Main block
         └──Type expr: Foo
         └──Expr: Function App
            └──Type expr: Foo
            └──Function: test
            └──() |}]
