open Core
open Print_frontend_ir

let%expect_test "Simple class inheritance" =
  print_frontend_ir
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
         └──Field: VTable []
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
      └──Class: Baz
         └──Field: VTable []
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
         └──Field: Int
      └──Main expr |}]

let%expect_test "Access field of superclass" =
  print_frontend_ir
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
      x.f;
      x.g
    }
  " ;
  [%expect
    {|
      Program
      └──Class: Foo
         └──Field: VTable []
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
      └──Class: Baz
         └──Field: VTable []
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
         └──Field: Int
      └──Main expr
         └──Expr: Let var: _var_x0
            └──Expr: Constructor for: Baz
         └──Expr: Objfield: _var_x0[4]
         └──Expr: Objfield: _var_x0[5] |}]

let%expect_test "Access field of super-superclass" =
  print_frontend_ir
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
      x.f;
      x.g;
      x.h
    }
  " ;
  [%expect
    {|
      Program
      └──Class: Foo
         └──Field: VTable []
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
      └──Class: Baz
         └──Field: VTable []
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
         └──Field: Int
      └──Class: Banana
         └──Field: VTable []
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
         └──Field: Int
         └──Field: Int
      └──Main expr
         └──Expr: Let var: _var_x0
            └──Expr: Constructor for: Banana
         └──Expr: Objfield: _var_x0[4]
         └──Expr: Objfield: _var_x0[5]
         └──Expr: Objfield: _var_x0[6] |}]

let%expect_test "Access method of superclass" =
  print_frontend_ir
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
         └──Field: VTable [_Foo__getF]
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
      └──Class: Baz
         └──Field: VTable [_Foo__getF]
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
         └──Field: Int
      └── Function: _Foo__getF
         └── Return type: Int
         └──Param: Class: Foo this
         └──Body block
            └──Expr: Objfield: this[4]
      └──Main expr
         └──Expr: Let var: _var_x0
            └──Expr: Constructor for: Baz
         └──Expr: ObjMethod: _var_x0._Baz__getF
            └──() |}]

let%expect_test "Override method of superclass" =
  print_frontend_ir
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
         └──Field: VTable [_Foo__get]
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
      └──Class: Baz
         └──Field: VTable [_Baz__get]
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
         └──Field: Int
      └── Function: _Foo__get
         └── Return type: Int
         └──Param: Class: Foo this
         └──Body block
            └──Expr: Objfield: this[4]
      └── Function: _Baz__get
         └── Return type: Int
         └──Param: Class: Baz this
         └──Body block
            └──Expr: Objfield: this[5]
      └──Main expr
         └──Expr: Let var: _var_x0
            └──Expr: Constructor for: Baz
         └──Expr: ObjMethod: _var_x0._Baz__get
            └──() |}]

let%expect_test "Overload method of superclass" =
  print_frontend_ir
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
         └──Field: VTable [_Foo__get]
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
      └──Class: Baz
         └──Field: VTable [_Foo__get, _Baz__geti]
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
         └──Field: Int
      └── Function: _Foo__get
         └── Return type: Int
         └──Param: Class: Foo this
         └──Body block
            └──Expr: Objfield: this[4]
      └── Function: _Baz__geti
         └── Return type: Void
         └──Param: Class: Baz this
         └──Param: Int f
         └──Body block
            └──Expr: Objfield: this[5]
      └──Main expr
         └──Expr: Let var: _var_x0
            └──Expr: Constructor for: Baz
         └──Expr: ObjMethod: _var_x0._Baz__geti
            └──Expr: Int:1 |}]

let%expect_test "Override method of superclass" =
  print_frontend_ir
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
         └──Field: VTable [_Foo__get]
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
      └──Class: Baz
         └──Field: VTable [_Baz__get]
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
         └──Field: Int
      └── Function: _Foo__get
         └── Return type: Int
         └──Param: Class: Foo this
         └──Body block
            └──Expr: Objfield: this[4]
      └── Function: _Baz__get
         └── Return type: Int
         └──Param: Class: Baz this
         └──Body block
            └──Expr: Objfield: this[5]
      └──Main expr
         └──Expr: Let var: _var_x0
            └──Expr: Constructor for: Baz
         └──Expr: ObjMethod: _var_x0._Baz__get
            └──() |}]

let%expect_test "Overload method of superclass" =
  print_frontend_ir
    " 
    class Foo {
      capability linear Bar;
      var int f : Bar;


      void test(int f) : Bar{
        this.f := f
      }

      void get() : Bar{
        this.f
      }
      void something(int f) : Bar{
        this.f := f
      }
    }
    class Baz extends Foo {
      capability linear Boo;
      var int g : Boo;
      void get(int f) : Boo{
        this.g
      }
      void get() : Boo{
        this.g
      }
      void set(int f) : Boo{
        this.g := f
      }
    }
    function void treatAsFoo(Foo x){
       x.get() //should be same index
    }
    void main() {
      let x = new Baz();
      x.get() // same index regardless if Foo or Baz
    }
  " ;
  [%expect
    {|
      Program
      └──Class: Foo
         └──Field: VTable [_Foo__testi, _Foo__get, _Foo__somethingi]
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
      └──Class: Baz
         └──Field: VTable [_Foo__testi, _Baz__get, _Foo__somethingi, _Baz__geti, _Baz__seti]
         └──Field: ThreadLocal ID
         └──Field: Read Lock Counter
         └──Field: Write Lock Counter
         └──Field: Int
         └──Field: Int
      └── Function: treatAsFoo
         └── Return type: Void
         └──Param: Class: Foo x
         └──Body block
            └──Expr: ObjMethod: x._Foo__get
               └──()
      └── Function: _Foo__testi
         └── Return type: Void
         └──Param: Class: Foo this
         └──Param: Int f
         └──Body block
            └──Expr: Assign
               └──Expr: Objfield: this[4]
               └──Expr: Variable: f
      └── Function: _Foo__get
         └── Return type: Void
         └──Param: Class: Foo this
         └──Body block
            └──Expr: Objfield: this[4]
      └── Function: _Foo__somethingi
         └── Return type: Void
         └──Param: Class: Foo this
         └──Param: Int f
         └──Body block
            └──Expr: Assign
               └──Expr: Objfield: this[4]
               └──Expr: Variable: f
      └── Function: _Baz__geti
         └── Return type: Void
         └──Param: Class: Baz this
         └──Param: Int f
         └──Body block
            └──Expr: Objfield: this[5]
      └── Function: _Baz__get
         └── Return type: Void
         └──Param: Class: Baz this
         └──Body block
            └──Expr: Objfield: this[5]
      └── Function: _Baz__seti
         └── Return type: Void
         └──Param: Class: Baz this
         └──Param: Int f
         └──Body block
            └──Expr: Assign
               └──Expr: Objfield: this[5]
               └──Expr: Variable: f
      └──Main expr
         └──Expr: Let var: _var_x0
            └──Expr: Constructor for: Baz
         └──Expr: ObjMethod: _var_x0._Baz__get
            └──() |}]
