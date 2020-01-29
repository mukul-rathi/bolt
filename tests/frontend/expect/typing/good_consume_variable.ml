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
          └──Expr: Block
             └──Type expr: Int
             └──Expr: If
                └──Type expr: Int
                └──Expr: Bool:false
                └──Expr: Block
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
                   └──Expr: Objfield: (Class: Choco) _var_y0.f
                      └──Type expr: Int
                └──Expr: Block
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
                   └──Expr: Variable: _var_y0
                      └──Type expr: Int |}]

let%expect_test "Access object after consumption of field" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;

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
       └──Regions:
          └──Region: Linear Bar
       └──Field Defn: f
          └──Mode: Const
          └──Type expr: Int
          └──Regions: Bar
    └──Expr: Block
       └──Type expr: Class: Foo
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Consume
          └──Expr: Objfield: (Class: Foo) _var_x0.f
             └──Type expr: Int
       └──Expr: Variable: _var_x0
          └──Type expr: Class: Foo |}]

let%expect_test "Access other field after consumption of field" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;
      const int g : Bar;

    }
    void main(){
      let x = new Foo();
      consume x.f;
      x.g
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
    └──Expr: Block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Consume
          └──Expr: Objfield: (Class: Foo) _var_x0.f
             └──Type expr: Int
       └──Expr: Objfield: (Class: Foo) _var_x0.g
          └──Type expr: Int |}]

let%expect_test "Access method after consumption of field" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;
      int test() : Bar { 42 }
    }
    void main(){
      let x = new Foo();
      consume x.f;
      x.test()
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
       └── Method: test
          └── Return type: Int
          └──Param: Void
          └── Effect regions
          └──   Regions: Bar
          └──Expr: Block
             └──Type expr: Int
             └──Expr: Int:42
    └──Expr: Block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Consume
          └──Expr: Objfield: (Class: Foo) _var_x0.f
             └──Type expr: Int
       └──Expr: ObjMethod: (Class: Foo) _var_x0.test
          └──Type expr: Int
          └──() |}]

let%expect_test "Access field in method after consumption of field" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;
      int test() : Bar { this.f } (* this.f has been consumed, but we can't tell this 
      locally, so would be accepted *)
    }
    void main(){
      let x = new Foo();
      consume x.f;
      x.test()
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
       └── Method: test
          └── Return type: Int
          └──Param: Void
          └── Effect regions
          └──   Regions: Bar
          └──Expr: Block
             └──Type expr: Int
             └──Expr: Objfield: (Class: Foo) this.f
                └──Type expr: Int
    └──Expr: Block
       └──Type expr: Int
       └──Expr: Let var: _var_x0
          └──Type expr: Class: Foo
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
       └──Expr: Consume
          └──Expr: Objfield: (Class: Foo) _var_x0.f
             └──Type expr: Int
       └──Expr: ObjMethod: (Class: Foo) _var_x0.test
          └──Type expr: Int
          └──() |}]

let%expect_test "Access variable after consumed then reassigned" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;
      int test() : Bar { this.f } (* this.f has been consumed, but we can't tell this 
      locally, so would be accepted *)
    }
    void main(){
      let x = new Foo();
      consume x;
      x := new Foo()    
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
         └── Method: test
            └── Return type: Int
            └──Param: Void
            └── Effect regions
            └──   Regions: Bar
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Foo) this.f
                  └──Type expr: Int
      └──Expr: Block
         └──Type expr: Class: Foo
         └──Expr: Let var: _var_x0
            └──Type expr: Class: Foo
            └──Expr: Constructor for: Foo
               └──Type expr: Class: Foo
         └──Expr: Consume
            └──Expr: Variable: _var_x0
               └──Type expr: Class: Foo
         └──Expr: Assign
            └──Type expr: Class: Foo
            └──Expr: Variable: _var_x0
               └──Type expr: Class: Foo
            └──Expr: Constructor for: Foo
               └──Type expr: Class: Foo

|}]

let%expect_test "Access variable after consumed then shadowed in an inner scope" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
      const int f : Bar;
      int test() : Bar { this.f } (* this.f has been consumed, but we can't tell this 
      locally, so would be accepted *)
    }
    void main(){
      let x = new Foo();
      consume x;
      if (true){
        let x = 42;
        x (* this access is fine as shadowed *)
      } 
      else{
        42
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
         └── Method: test
            └── Return type: Int
            └──Param: Void
            └── Effect regions
            └──   Regions: Bar
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Foo) this.f
                  └──Type expr: Int
      └──Expr: Block
         └──Type expr: Int
         └──Expr: Let var: _var_x0
            └──Type expr: Class: Foo
            └──Expr: Constructor for: Foo
               └──Type expr: Class: Foo
         └──Expr: Consume
            └──Expr: Variable: _var_x0
               └──Type expr: Class: Foo
         └──Expr: If
            └──Type expr: Int
            └──Expr: Bool:true
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Let var: _var_x1
                  └──Type expr: Int
                  └──Expr: Int:42
               └──Expr: Variable: _var_x1
                  └──Type expr: Int
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Int:42
|}]

let%expect_test "Consume shared variable if only accessed by one thread" =
  print_typed_ast
    " 
    class Foo {
      region linear Bar;
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
            consume x (* note accessed in only one thread *)
         }
         y.f := 5
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
            └──Mode: Var
            └──Type expr: Int
            └──Regions: Bar
         └── Method: test
            └── Return type: Int
            └──Param: Void
            └── Effect regions
            └──   Regions: Bar
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Objfield: (Class: Foo) this.f
                  └──Type expr: Int
      └──Expr: Block
         └──Type expr: Int
         └──Expr: Let var: _var_x0
            └──Type expr: Class: Foo
            └──Expr: Constructor for: Foo
               └──Type expr: Class: Foo
         └──Expr: Let var: _var_y0
            └──Type expr: Class: Foo
            └──Expr: Constructor for: Foo
               └──Type expr: Class: Foo
         └──Expr: Finish_async
            └──Type expr: Int
            └── Async Expr:
               └──Expr: Block
                  └──Type expr: Class: Foo
                  └──Expr: While
                     └──Type expr: Void
                     └──Expr: Bin Op: <
                        └──Type expr: Bool
                        └──Expr: ObjMethod: (Class: Foo) _var_x0.test
                           └──Type expr: Int
                           └──()
                        └──Expr: Int:10
                     └──Expr: Block
                        └──Type expr: Int
                        └──Expr: Assign
                           └──Type expr: Int
                           └──Expr: Objfield: (Class: Foo) _var_x0.f
                              └──Type expr: Int
                           └──Expr: Bin Op: +
                              └──Type expr: Int
                              └──Expr: Objfield: (Class: Foo) _var_x0.f
                                 └──Type expr: Int
                              └──Expr: Int:1
                  └──Expr: Consume
                     └──Expr: Variable: _var_x0
                        └──Type expr: Class: Foo
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Assign
                  └──Type expr: Int
                  └──Expr: Objfield: (Class: Foo) _var_y0.f
                     └──Type expr: Int
                  └──Expr: Int:5
|}]
