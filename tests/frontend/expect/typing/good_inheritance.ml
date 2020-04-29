open Core
open Print_typed_ast

let%expect_test "Simple class inheritance" =
  print_typed_ast
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
      └──Class: Baz
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
      └──Main block
         └──Type expr: Void |}]

let%expect_test "Access field of superclass" =
  print_typed_ast
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
      └──Class: Baz
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: x
            └──Type expr: Baz
            └──Expr: Constructor for: Baz
               └──Type expr: Baz
         └──Expr: Objfield: (Class: Baz) x.f
            └──Type expr: Int |}]

let%expect_test "Access field of super-superclass" =
  print_typed_ast
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
      └──Class: Baz
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
      └──Class: Banana
         └──Capabilities:
            └──Capability: Read Haha
         └──Field Defn: h
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Haha
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: x
            └──Type expr: Banana
            └──Expr: Constructor for: Banana
               └──Type expr: Banana
         └──Expr: Objfield: (Class: Banana) x.f
            └──Type expr: Int |}]

let%expect_test "Access method of superclass" =
  print_typed_ast
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
         └── Method: getF
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Foo) this.f
                  └──Type expr: Int
      └──Class: Baz
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: x
            └──Type expr: Baz
            └──Expr: Constructor for: Baz
               └──Type expr: Baz
         └──Expr: ObjMethod: (Class: Baz) x.getF
            └──Type expr: Int
            └──() |}]

let%expect_test "Override method of superclass" =
  print_typed_ast
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
         └── Method: get
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Foo) this.f
                  └──Type expr: Int
      └──Class: Baz
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
         └── Method: get
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Boo
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Baz) this.g
                  └──Type expr: Int
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: x
            └──Type expr: Baz
            └──Expr: Constructor for: Baz
               └──Type expr: Baz
         └──Expr: ObjMethod: (Class: Baz) x.get
            └──Type expr: Int
            └──() |}]

let%expect_test "Overload method of superclass" =
  print_typed_ast
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
         └── Method: get
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Foo) this.f
                  └──Type expr: Int
      └──Class: Baz
         └──Capabilities:
            └──Capability: Linear Boo
         └──Field Defn: g
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Boo
         └── Method: get
            └── Return type: Void
            └──Param: f
               └──Type expr: Int
            └── Used capabilities
            └──   Capabilities: Boo
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Baz) this.g
                  └──Type expr: Int
      └──Main block
         └──Type expr: Void
         └──Expr: Let var: x
            └──Type expr: Baz
            └──Expr: Constructor for: Baz
               └──Type expr: Baz
         └──Expr: ObjMethod: (Class: Baz) x.get
            └──Type expr: Void
            └──Expr: Int:1 |}]
