open Core
open Print_desugared_ast

let%expect_test "Function overloading different arg types" =
  print_desugared_ast
    "
   function int test(int f) {
        f+1
    }
    function int test(bool b){
      if b { 1} else {0}
    }

   void main() {
    test(12); 
    test(true)
  }

  " ;
  [%expect
    {|
    Program
    └── Function: _testi
       └── Return type: Int
       └──Param: f
          └──Type expr: Int
       └──Body block
          └──Type expr: Int
          └──Expr: Bin Op: +
             └──Type expr: Int
             └──Expr: Variable: f
                └──Type expr: Int
             └──Expr: Int:1
    └── Function: _testb
       └── Return type: Int
       └──Param: b
          └──Type expr: Bool
       └──Body block
          └──Type expr: Int
          └──Expr: If
             └──Type expr: Int
             └──Expr: Variable: b
                └──Type expr: Bool
             └──Then block
                └──Type expr: Int
                └──Expr: Int:1
             └──Else block
                └──Type expr: Int
                └──Expr: Int:0
    └──Main block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _testi
          └──Expr: Int:12
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _testb
          └──Expr: Bool:true |}]

let%expect_test "Function overloading different number of args" =
  print_desugared_ast
    "
   function int test() {
        1
    }
    function int test(bool b){
      if b { 1} else {0}
    }
  function int test(bool b, int x) {
      if b {x} else {-x}
    }
   void main() {
    test(); 
    test(true);
    test(true, 4)
  }

  " ;
  [%expect
    {|
    Program
    └── Function: _test
       └── Return type: Int
       └──Param: Void
       └──Body block
          └──Type expr: Int
          └──Expr: Int:1
    └── Function: _testb
       └── Return type: Int
       └──Param: b
          └──Type expr: Bool
       └──Body block
          └──Type expr: Int
          └──Expr: If
             └──Type expr: Int
             └──Expr: Variable: b
                └──Type expr: Bool
             └──Then block
                └──Type expr: Int
                └──Expr: Int:1
             └──Else block
                └──Type expr: Int
                └──Expr: Int:0
    └── Function: _testbi
       └── Return type: Int
       └──Param: b
          └──Type expr: Bool
       └──Param: x
          └──Type expr: Int
       └──Body block
          └──Type expr: Int
          └──Expr: If
             └──Type expr: Int
             └──Expr: Variable: b
                └──Type expr: Bool
             └──Then block
                └──Type expr: Int
                └──Expr: Variable: x
                   └──Type expr: Int
             └──Else block
                └──Type expr: Int
                └──Expr: Unary Op: -
                   └──Type expr: Int
                   └──Expr: Variable: x
                      └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _test
          └──()
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _testb
          └──Expr: Bool:true
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _testbi
          └──Expr: Bool:true
          └──Expr: Int:4 |}]

let%expect_test "Function overloading different order of args" =
  print_desugared_ast
    "
   function int test(int x, bool b) {
        if (b) {x} else {-x}
    }
    function int test(bool b, int x){
      if b { -x} else { x}
    }

   void main() {
    test(true, 5); 
    test(5,true)
  }

  " ;
  [%expect
    {|
    Program
    └── Function: _testib
       └── Return type: Int
       └──Param: x
          └──Type expr: Int
       └──Param: b
          └──Type expr: Bool
       └──Body block
          └──Type expr: Int
          └──Expr: If
             └──Type expr: Int
             └──Expr: Variable: b
                └──Type expr: Bool
             └──Then block
                └──Type expr: Int
                └──Expr: Variable: x
                   └──Type expr: Int
             └──Else block
                └──Type expr: Int
                └──Expr: Unary Op: -
                   └──Type expr: Int
                   └──Expr: Variable: x
                      └──Type expr: Int
    └── Function: _testbi
       └── Return type: Int
       └──Param: b
          └──Type expr: Bool
       └──Param: x
          └──Type expr: Int
       └──Body block
          └──Type expr: Int
          └──Expr: If
             └──Type expr: Int
             └──Expr: Variable: b
                └──Type expr: Bool
             └──Then block
                └──Type expr: Int
                └──Expr: Unary Op: -
                   └──Type expr: Int
                   └──Expr: Variable: x
                      └──Type expr: Int
             └──Else block
                └──Type expr: Int
                └──Expr: Variable: x
                   └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _testbi
          └──Expr: Bool:true
          └──Expr: Int:5
       └──Expr: Function App
          └──Type expr: Int
          └──Function: _testib
          └──Expr: Int:5
          └──Expr: Bool:true |}]

let%expect_test "Method overloading different arg types" =
  print_desugared_ast
    "
    class Foo{
      capability read readCap;
      var int f : readCap;
       int test(int x) : readCap {
        this.f + x
       }
    int test(bool b) : readCap {
      if b { this.f + 1 } else { this.f }
    }
  }
   void main() {
     let x = new Foo();
    x.test(12); 
    x.test(true)
  }

  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Read readCap
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: readCap
       └── Method: _testi
          └── Return type: Int
          └──Param: x
             └──Type expr: Int
          └── Used capabilities
          └──   Capabilities: readCap
          └──Body block
             └──Type expr: Int
             └──Expr: Bin Op: +
                └──Type expr: Int
                └──Expr: Objfield: (Class: Foo) this.f
                   └──Type expr: Int
                   └──Capabilities:
                      └──Capability: Read readCap
                └──Expr: Variable: x
                   └──Type expr: Int
       └── Method: _testb
          └── Return type: Int
          └──Param: b
             └──Type expr: Bool
          └── Used capabilities
          └──   Capabilities: readCap
          └──Body block
             └──Type expr: Int
             └──Expr: If
                └──Type expr: Int
                └──Expr: Variable: b
                   └──Type expr: Bool
                └──Then block
                   └──Type expr: Int
                   └──Expr: Bin Op: +
                      └──Type expr: Int
                      └──Expr: Objfield: (Class: Foo) this.f
                         └──Type expr: Int
                         └──Capabilities:
                            └──Capability: Read readCap
                      └──Expr: Int:1
                └──Else block
                   └──Type expr: Int
                   └──Expr: Objfield: (Class: Foo) this.f
                      └──Type expr: Int
                      └──Capabilities:
                         └──Capability: Read readCap
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: ObjMethod: (Class: Foo) _var_x0._testi
          └── Possible Capabilities:
             └── Possible Capability: Read readCap
          └──Type expr: Int
          └──Expr: Int:12
       └──Expr: ObjMethod: (Class: Foo) _var_x0._testb
          └── Possible Capabilities:
             └── Possible Capability: Read readCap
          └──Type expr: Int
          └──Expr: Bool:true |}]

let%expect_test "Method overloading different number of args" =
  print_desugared_ast
    "
    class Foo{
      capability read readCap;
      var int f : readCap;
       int test() : readCap {
        this.f
       }
    int test(bool b) : readCap {
      if b { this.f + 1 } else { this.f }
    }
    int test(bool b, int x) : readCap{
      if b {x} else {this.f}
    }
  }
   void main() {
     let x= new Foo();
     x.test(); 
     x.test(true);
     x.test(true, 42)
  }

  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Read readCap
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: readCap
       └── Method: _test
          └── Return type: Int
          └──Param: Void
          └── Used capabilities
          └──   Capabilities: readCap
          └──Body block
             └──Type expr: Int
             └──Expr: Objfield: (Class: Foo) this.f
                └──Type expr: Int
                └──Capabilities:
                   └──Capability: Read readCap
       └── Method: _testb
          └── Return type: Int
          └──Param: b
             └──Type expr: Bool
          └── Used capabilities
          └──   Capabilities: readCap
          └──Body block
             └──Type expr: Int
             └──Expr: If
                └──Type expr: Int
                └──Expr: Variable: b
                   └──Type expr: Bool
                └──Then block
                   └──Type expr: Int
                   └──Expr: Bin Op: +
                      └──Type expr: Int
                      └──Expr: Objfield: (Class: Foo) this.f
                         └──Type expr: Int
                         └──Capabilities:
                            └──Capability: Read readCap
                      └──Expr: Int:1
                └──Else block
                   └──Type expr: Int
                   └──Expr: Objfield: (Class: Foo) this.f
                      └──Type expr: Int
                      └──Capabilities:
                         └──Capability: Read readCap
       └── Method: _testbi
          └── Return type: Int
          └──Param: b
             └──Type expr: Bool
          └──Param: x
             └──Type expr: Int
          └── Used capabilities
          └──   Capabilities: readCap
          └──Body block
             └──Type expr: Int
             └──Expr: If
                └──Type expr: Int
                └──Expr: Variable: b
                   └──Type expr: Bool
                └──Then block
                   └──Type expr: Int
                   └──Expr: Variable: x
                      └──Type expr: Int
                └──Else block
                   └──Type expr: Int
                   └──Expr: Objfield: (Class: Foo) this.f
                      └──Type expr: Int
                      └──Capabilities:
                         └──Capability: Read readCap
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: ObjMethod: (Class: Foo) _var_x0._test
          └── Possible Capabilities:
             └── Possible Capability: Read readCap
          └──Type expr: Int
          └──()
       └──Expr: ObjMethod: (Class: Foo) _var_x0._testb
          └── Possible Capabilities:
             └── Possible Capability: Read readCap
          └──Type expr: Int
          └──Expr: Bool:true
       └──Expr: ObjMethod: (Class: Foo) _var_x0._testbi
          └── Possible Capabilities:
             └── Possible Capability: Read readCap
          └──Type expr: Int
          └──Expr: Bool:true
          └──Expr: Int:42 |}]

let%expect_test "Method overloading different order of args" =
  print_desugared_ast
    "
    class Foo{
      capability read readCap;
      var int f : readCap;
       int test(int x, bool b) : readCap {
        if (b) {x} else {this.f}
    }
       int test(bool b, int x) : readCap {
      if b { this.f} else { x}
     }
  }
   void main() {
     let x = new Foo(f:10);
     x.test(true, 5); 
    x.test(5,true)
  }

  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──Capabilities:
          └──Capability: Read readCap
       └──Field Defn: f
          └──Modifier: Var
          └──Type expr: Int
          └──Capabilities: readCap
       └── Method: _testib
          └── Return type: Int
          └──Param: x
             └──Type expr: Int
          └──Param: b
             └──Type expr: Bool
          └── Used capabilities
          └──   Capabilities: readCap
          └──Body block
             └──Type expr: Int
             └──Expr: If
                └──Type expr: Int
                └──Expr: Variable: b
                   └──Type expr: Bool
                └──Then block
                   └──Type expr: Int
                   └──Expr: Variable: x
                      └──Type expr: Int
                └──Else block
                   └──Type expr: Int
                   └──Expr: Objfield: (Class: Foo) this.f
                      └──Type expr: Int
                      └──Capabilities:
                         └──Capability: Read readCap
       └── Method: _testbi
          └── Return type: Int
          └──Param: b
             └──Type expr: Bool
          └──Param: x
             └──Type expr: Int
          └── Used capabilities
          └──   Capabilities: readCap
          └──Body block
             └──Type expr: Int
             └──Expr: If
                └──Type expr: Int
                └──Expr: Variable: b
                   └──Type expr: Bool
                └──Then block
                   └──Type expr: Int
                   └──Expr: Objfield: (Class: Foo) this.f
                      └──Type expr: Int
                      └──Capabilities:
                         └──Capability: Read readCap
                └──Else block
                   └──Type expr: Int
                   └──Expr: Variable: x
                      └──Type expr: Int
    └──Main block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:10
       └──Expr: ObjMethod: (Class: Foo) _var_x0._testbi
          └── Possible Capabilities:
             └── Possible Capability: Read readCap
          └──Type expr: Int
          └──Expr: Bool:true
          └──Expr: Int:5
       └──Expr: ObjMethod: (Class: Foo) _var_x0._testib
          └── Possible Capabilities:
             └── Possible Capability: Read readCap
          └──Type expr: Int
          └──Expr: Int:5
          └──Expr: Bool:true |}]
