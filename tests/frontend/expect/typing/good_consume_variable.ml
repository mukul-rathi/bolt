open Core
open Print_typed_ast

let%expect_test "Consume variable" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      const int f : Bar;
      const int g : Bar ; 
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
      if true {
        let x = new Foo(f:4, g:5, h:6);
        let y = consume x; // Consume linear variable 
        let z = 5;
        let w = consume z; // Can consume an int 
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
  [%expect {|
    Line:33 Position:18 Type error - Trying to consume a const field. |}]

let%expect_test "Access object after consumption of field" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;

    }
    void main(){
      let x = new Foo();
      consume x.f;
      x
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
    └──Main block
       └──Type expr: Class: Foo
       └──Expr: Let var: x
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Consume
          └──Expr: Objfield: (Class: Foo) x.f
             └──Type expr: Int
       └──Expr: Variable: x
          └──Type expr: Class: Foo |}]

let%expect_test "Access other field after consumption of field" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      const int f : Bar;
      const int g : Bar;

    }
    void main(){
      let x = new Foo();
      consume x.f;
      x.g
    }
  " ;
  [%expect {|
    Line:10 Position:7 Type error - Trying to consume a const field. |}]

let%expect_test "Access method after consumption of field" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      const int f : Bar;
      int test() : Bar { 42 }
    }
    void main(){
      let x = new Foo();
      consume x.f;
      x.test()
    }
  " ;
  [%expect {|
    Line:9 Position:7 Type error - Trying to consume a const field. |}]

let%expect_test "Access field in method after consumption of field" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      const int f : Bar;
      int test() : Bar { this.f } // this.f has been consumed, but we can't tell this 
      locally, so would be accepted 
    }
    void main(){
      let x = new Foo();
      consume x.f;
      x.test()
    }
  " ;
  [%expect {|
    Line:6 Position:15: syntax error |}]

let%expect_test "Access variable after consumed then reassigned" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      const int f : Bar;
      int test() : Bar { this.f } // this.f has been consumed, but we can't tell this 
      locally, so would be accepted 
    }
    void main(){
      let x = new Foo();
      consume x;
      x := new Foo()    
    }
  " ;
  [%expect {|
      Line:6 Position:15: syntax error

|}]

let%expect_test "Access variable after consumed then shadowed in an inner scope" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      const int f : Bar;
      int test() : Bar { this.f } // this.f has been consumed, but we can't tell this 
      locally, so would be accepted 
    }
    void main(){
      let x = new Foo();
      consume x;
      if (true){
        let x = 42;
        x // this access is fine as shadowed 
      } 
      else{
        42
      }  
    }
  " ;
  [%expect {|
      Line:6 Position:15: syntax error
|}]

let%expect_test "Consume shared variable if only accessed by one local" =
  print_typed_ast
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;
      int test() : Bar { this.f }
    }
    void main(){
      let x = new Foo();
      let y = new Foo();
      finish{
         async{
            while((x.test()) < 10){
               x.f := x.f +1 
            };
            consume x // note accessed in only one local 
         }
         y.f := 5
      }
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
         └── Method: test
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Foo) this.f
                  └──Type expr: Int
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: x
            └──Type expr: Class: Foo
            └──Expr: Constructor for: Foo
               └──Type expr: Class: Foo
         └──Expr: Let var: y
            └──Type expr: Class: Foo
            └──Expr: Constructor for: Foo
               └──Type expr: Class: Foo
         └──Expr: Finish_async
            └──Type expr: Int
               └──Async Expr block
                  └──Type expr: Class: Foo
                  └──Expr: While
                     └──Type expr: Void
                     └──Expr: Bin Op: <
                        └──Type expr: Bool
                        └──Expr: ObjMethod: (Class: Foo) x.test
                           └──Type expr: Int
                           └──()
                        └──Expr: Int:10
                     └──Body block
                        └──Type expr: Int
                        └──Expr: Assign
                           └──Type expr: Int
                           └──Expr: Objfield: (Class: Foo) x.f
                              └──Type expr: Int
                           └──Expr: Bin Op: +
                              └──Type expr: Int
                              └──Expr: Objfield: (Class: Foo) x.f
                                 └──Type expr: Int
                              └──Expr: Int:1
                  └──Expr: Consume
                     └──Expr: Variable: x
                        └──Type expr: Class: Foo
            └──Current thread block
               └──Type expr: Int
               └──Expr: Assign
                  └──Type expr: Int
                  └──Expr: Objfield: (Class: Foo) y.f
                     └──Type expr: Int
                  └──Expr: Int:5
|}]
