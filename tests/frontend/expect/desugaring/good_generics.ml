open Core
open Print_desugared_ast

let%expect_test "Generic class that is uninstantiated is dropped" =
  print_desugared_ast
    " 
    class Foo<T> {
      capability linear Bar;
      var T f : Bar;
    }
    void main() {
    }
  " ;
  [%expect
    {|
      Program
      └──Main block
         └──Type expr: Void |}]

let%expect_test "Generic field assigned different types depending on instantiation" =
  print_desugared_ast
    " 
    class Foo<T> {
      capability linear Bar;
      var T f : Bar;
    }
    void main() {
      let x = new Foo<int>(f:100);
      let y = new Foo<bool>(f:true)

    }
  " ;
  [%expect
    {|
      Program
      └──Class: _FooBool
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Bool
            └──Capabilities: Bar
      └──Class: _FooInt
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Bar
      └──Main block
         └──Type expr: _FooBool
         └──Expr: Let var: _var_x0
            └──Type expr: _FooInt
            └──Expr: Constructor for: _FooInt
               └──Type expr: _FooInt
               └── Field: f
                  └──Type expr: Int
                  └──Expr: Int:100
         └──Expr: Let var: _var_y0
            └──Type expr: _FooBool
            └──Expr: Constructor for: _FooBool
               └──Type expr: _FooBool
               └── Field: f
                  └──Type expr: Bool
                  └──Expr: Bool:true |}]

let%expect_test "Method takes in generic type of same type as argument" =
  print_desugared_ast
    " 
    class Foo<T>{
      capability linear Bar;
      var T f : Bar;
      void copy(Foo<T> x): Bar{
        this.f := x.f
      }
    }
    void main() {
      let x =  new Foo<int>(f:5);
      let y =  new Foo<int>();
      y.copy(consume x)
    }
  " ;
  [%expect
    {|
      Program
      └──Class: _FooInt
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Bar
         └── Method: _copy7_FooInt
            └── Return type: Void
            └──Param: x
               └──Type expr: _FooInt
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Assign
                  └──Type expr: Int
                  └──Expr: Objfield: (Class: _FooInt) this.f
                     └──Type expr: Int
                     └──Capabilities:
                        └──Capability: Linear Bar
                  └──Expr: Objfield: (Class: _FooInt) x.f
                     └──Type expr: Int
                     └──Capabilities:
                        └──Capability: Linear Bar
      └──Class: _FooInt
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Bar
         └── Method: _copy7_FooInt
            └── Return type: Void
            └──Param: x
               └──Type expr: _FooInt
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Assign
                  └──Type expr: Int
                  └──Expr: Objfield: (Class: _FooInt) this.f
                     └──Type expr: Int
                     └──Capabilities:
                        └──Capability: Linear Bar
                  └──Expr: Objfield: (Class: _FooInt) x.f
                     └──Type expr: Int
                     └──Capabilities:
                        └──Capability: Linear Bar
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: _var_x0
            └──Type expr: _FooInt
            └──Expr: Constructor for: _FooInt
               └──Type expr: _FooInt
               └── Field: f
                  └──Type expr: Int
                  └──Expr: Int:5
         └──Expr: Let var: _var_y0
            └──Type expr: _FooInt
            └──Expr: Constructor for: _FooInt
               └──Type expr: _FooInt
         └──Expr: ObjMethod: (Class: _FooInt) _var_y0._copy7_FooInt
            └── Possible Capabilities:
               └── Possible Capability: Linear Bar
            └──Type expr: Int
            └──Expr: Consume
               └──Expr: Variable: _var_x0
                  └──Type expr: _FooInt
                  └── Possible Capabilities:
                     └── Possible Capability: Linear Bar |}]

let%expect_test "Larger generics example" =
  print_desugared_ast
    " 
    class Foo<T>{
      capability linear Bar;
      var T f : Bar;
      void copy(Foo<T> x): Bar{
        finish{
           async{
              this.f := x.f
           }
           let y = new Foo<int>(f:0);
           for (let i=0; i < 100; i:=i+1){
              y.setF((y.getF()) + i) 
           };
           printf(\"Value of y: %d\", y.getF() )
        }
      }
      void setF(T f) : Bar {
         this.f := f
      }
      T getF() : Bar {
         this.f
      }

      void baz() : Bar{
         let z = getTrueFoo();
         if (!(z.f)){
            z.setF(true)

         }
         else{
            z.setF(false)
         }
      }
    }

   function Foo<bool> getTrueFoo(){
      new Foo<bool>(f:true)
   }


    void main() {
      let x =  new Foo<int>(f:5);
      let y =  new Foo<int>();
      y.copy(consume x) 
    }
  " ;
  [%expect
    {|
      Program
      └──Class: _FooBool
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Bool
            └──Capabilities: Bar
         └── Method: _copy8_FooBool
            └── Return type: Void
            └──Param: x
               └──Type expr: _FooBool
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Void
               └──Expr: Finish_async
                  └──Type expr: Void
                     └── Async Expr Free Vars:
                        └── (_FooBool) this, Capabilities: Bar
                        └── (_FooBool) x, Capabilities: Bar
                     └──Async Expr block
                        └──Type expr: Bool
                        └──Expr: Assign
                           └──Type expr: Bool
                           └──Expr: Objfield: (Class: _FooBool) this.f
                              └──Type expr: Bool
                              └──Capabilities:
                                 └──Capability: Linear Bar
                           └──Expr: Objfield: (Class: _FooBool) x.f
                              └──Type expr: Bool
                              └──Capabilities:
                                 └──Capability: Linear Bar
               └── Current ThreadLocal Expr Free Vars:
                  └──Current thread block
                     └──Type expr: Void
                     └──Expr: Let var: _var_y0
                        └──Type expr: _FooInt
                        └──Expr: Constructor for: _FooInt
                           └──Type expr: _FooInt
                           └── Field: f
                              └──Type expr: Int
                              └──Expr: Int:0
                        └──Expr: Let var: _var_i0
                           └──Type expr: Int
                           └──Expr: Int:0
                        └──Expr: While
                           └──Type expr: Void
                           └──Expr: Bin Op: <
                              └──Type expr: Bool
                              └──Expr: Variable: _var_i0
                                 └──Type expr: Int
                              └──Expr: Int:100
                           └──Body block
                              └──Type expr: Int
                              └──Expr: ObjMethod: (Class: _FooInt) _var_y0._setFi
                                 └── Possible Capabilities:
                                    └── Possible Capability: Linear Bar
                                 └──Type expr: Int
                                 └──Expr: Bin Op: +
                                    └──Type expr: Int
                                    └──Expr: ObjMethod: (Class: _FooInt) _var_y0._getF
                                       └── Possible Capabilities:
                                          └── Possible Capability: Linear Bar
                                       └──Type expr: Int
                                       └──()
                                    └──Expr: Variable: _var_i0
                                       └──Type expr: Int
                              └──Expr: Assign
                                 └──Type expr: Int
                                 └──Expr: Variable: _var_i0
                                    └──Type expr: Int
                                 └──Expr: Bin Op: +
                                    └──Type expr: Int
                                    └──Expr: Variable: _var_i0
                                       └──Type expr: Int
                                    └──Expr: Int:1
                     └──Expr: Printf
                        └──Value of y: %d
                        └──Expr: ObjMethod: (Class: _FooInt) _var_y0._getF
                           └── Possible Capabilities:
                              └── Possible Capability: Linear Bar
                           └──Type expr: Int
                           └──()
         └── Method: _setFb
            └── Return type: Void
            └──Param: f
               └──Type expr: Bool
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Bool
               └──Expr: Assign
                  └──Type expr: Bool
                  └──Expr: Objfield: (Class: _FooBool) this.f
                     └──Type expr: Bool
                     └──Capabilities:
                        └──Capability: Linear Bar
                  └──Expr: Variable: f
                     └──Type expr: Bool
         └── Method: _getF
            └── Return type: Bool
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Bool
               └──Expr: Objfield: (Class: _FooBool) this.f
                  └──Type expr: Bool
                  └──Capabilities:
                     └──Capability: Linear Bar
         └── Method: _baz
            └── Return type: Void
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Bool
               └──Expr: Let var: _var_z0
                  └──Type expr: _FooBool
                  └──Expr: Function App
                     └──Type expr: _FooBool
                     └──Function: getTrueFoo
                     └──()
               └──Expr: If
                  └──Type expr: Bool
                  └──Expr: Unary Op: !
                     └──Type expr: Bool
                     └──Expr: Objfield: (Class: _FooBool) _var_z0.f
                        └──Type expr: Bool
                        └──Capabilities:
                           └──Capability: Linear Bar
                  └──Then block
                     └──Type expr: Bool
                     └──Expr: ObjMethod: (Class: _FooBool) _var_z0._setFb
                        └── Possible Capabilities:
                           └── Possible Capability: Linear Bar
                        └──Type expr: Bool
                        └──Expr: Bool:true
                  └──Else block
                     └──Type expr: Bool
                     └──Expr: ObjMethod: (Class: _FooBool) _var_z0._setFb
                        └── Possible Capabilities:
                           └── Possible Capability: Linear Bar
                        └──Type expr: Bool
                        └──Expr: Bool:false
      └──Class: _FooInt
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Bar
         └── Method: _copy7_FooInt
            └── Return type: Void
            └──Param: x
               └──Type expr: _FooInt
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Void
               └──Expr: Finish_async
                  └──Type expr: Void
                     └── Async Expr Free Vars:
                        └── (_FooInt) this, Capabilities: Bar
                        └── (_FooInt) x, Capabilities: Bar
                     └──Async Expr block
                        └──Type expr: Int
                        └──Expr: Assign
                           └──Type expr: Int
                           └──Expr: Objfield: (Class: _FooInt) this.f
                              └──Type expr: Int
                              └──Capabilities:
                                 └──Capability: Linear Bar
                           └──Expr: Objfield: (Class: _FooInt) x.f
                              └──Type expr: Int
                              └──Capabilities:
                                 └──Capability: Linear Bar
               └── Current ThreadLocal Expr Free Vars:
                  └──Current thread block
                     └──Type expr: Void
                     └──Expr: Let var: _var_y0
                        └──Type expr: _FooInt
                        └──Expr: Constructor for: _FooInt
                           └──Type expr: _FooInt
                           └── Field: f
                              └──Type expr: Int
                              └──Expr: Int:0
                        └──Expr: Let var: _var_i0
                           └──Type expr: Int
                           └──Expr: Int:0
                        └──Expr: While
                           └──Type expr: Void
                           └──Expr: Bin Op: <
                              └──Type expr: Bool
                              └──Expr: Variable: _var_i0
                                 └──Type expr: Int
                              └──Expr: Int:100
                           └──Body block
                              └──Type expr: Int
                              └──Expr: ObjMethod: (Class: _FooInt) _var_y0._setFi
                                 └── Possible Capabilities:
                                    └── Possible Capability: Linear Bar
                                 └──Type expr: Int
                                 └──Expr: Bin Op: +
                                    └──Type expr: Int
                                    └──Expr: ObjMethod: (Class: _FooInt) _var_y0._getF
                                       └── Possible Capabilities:
                                          └── Possible Capability: Linear Bar
                                       └──Type expr: Int
                                       └──()
                                    └──Expr: Variable: _var_i0
                                       └──Type expr: Int
                              └──Expr: Assign
                                 └──Type expr: Int
                                 └──Expr: Variable: _var_i0
                                    └──Type expr: Int
                                 └──Expr: Bin Op: +
                                    └──Type expr: Int
                                    └──Expr: Variable: _var_i0
                                       └──Type expr: Int
                                    └──Expr: Int:1
                     └──Expr: Printf
                        └──Value of y: %d
                        └──Expr: ObjMethod: (Class: _FooInt) _var_y0._getF
                           └── Possible Capabilities:
                              └── Possible Capability: Linear Bar
                           └──Type expr: Int
                           └──()
         └── Method: _setFi
            └── Return type: Void
            └──Param: f
               └──Type expr: Int
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Assign
                  └──Type expr: Int
                  └──Expr: Objfield: (Class: _FooInt) this.f
                     └──Type expr: Int
                     └──Capabilities:
                        └──Capability: Linear Bar
                  └──Expr: Variable: f
                     └──Type expr: Int
         └── Method: _getF
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: _FooInt) this.f
                  └──Type expr: Int
                  └──Capabilities:
                     └──Capability: Linear Bar
         └── Method: _baz
            └── Return type: Void
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Bool
               └──Expr: Let var: _var_z0
                  └──Type expr: _FooBool
                  └──Expr: Function App
                     └──Type expr: _FooBool
                     └──Function: getTrueFoo
                     └──()
               └──Expr: If
                  └──Type expr: Bool
                  └──Expr: Unary Op: !
                     └──Type expr: Bool
                     └──Expr: Objfield: (Class: _FooBool) _var_z0.f
                        └──Type expr: Bool
                        └──Capabilities:
                           └──Capability: Linear Bar
                  └──Then block
                     └──Type expr: Bool
                     └──Expr: ObjMethod: (Class: _FooBool) _var_z0._setFb
                        └── Possible Capabilities:
                           └── Possible Capability: Linear Bar
                        └──Type expr: Bool
                        └──Expr: Bool:true
                  └──Else block
                     └──Type expr: Bool
                     └──Expr: ObjMethod: (Class: _FooBool) _var_z0._setFb
                        └── Possible Capabilities:
                           └── Possible Capability: Linear Bar
                        └──Type expr: Bool
                        └──Expr: Bool:false
      └──Class: _FooInt
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Bar
         └── Method: _copy7_FooInt
            └── Return type: Void
            └──Param: x
               └──Type expr: _FooInt
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Void
               └──Expr: Finish_async
                  └──Type expr: Void
                     └── Async Expr Free Vars:
                        └── (_FooInt) this, Capabilities: Bar
                        └── (_FooInt) x, Capabilities: Bar
                     └──Async Expr block
                        └──Type expr: Int
                        └──Expr: Assign
                           └──Type expr: Int
                           └──Expr: Objfield: (Class: _FooInt) this.f
                              └──Type expr: Int
                              └──Capabilities:
                                 └──Capability: Linear Bar
                           └──Expr: Objfield: (Class: _FooInt) x.f
                              └──Type expr: Int
                              └──Capabilities:
                                 └──Capability: Linear Bar
               └── Current ThreadLocal Expr Free Vars:
                  └──Current thread block
                     └──Type expr: Void
                     └──Expr: Let var: _var_y0
                        └──Type expr: _FooInt
                        └──Expr: Constructor for: _FooInt
                           └──Type expr: _FooInt
                           └── Field: f
                              └──Type expr: Int
                              └──Expr: Int:0
                        └──Expr: Let var: _var_i0
                           └──Type expr: Int
                           └──Expr: Int:0
                        └──Expr: While
                           └──Type expr: Void
                           └──Expr: Bin Op: <
                              └──Type expr: Bool
                              └──Expr: Variable: _var_i0
                                 └──Type expr: Int
                              └──Expr: Int:100
                           └──Body block
                              └──Type expr: Int
                              └──Expr: ObjMethod: (Class: _FooInt) _var_y0._setFi
                                 └── Possible Capabilities:
                                    └── Possible Capability: Linear Bar
                                 └──Type expr: Int
                                 └──Expr: Bin Op: +
                                    └──Type expr: Int
                                    └──Expr: ObjMethod: (Class: _FooInt) _var_y0._getF
                                       └── Possible Capabilities:
                                          └── Possible Capability: Linear Bar
                                       └──Type expr: Int
                                       └──()
                                    └──Expr: Variable: _var_i0
                                       └──Type expr: Int
                              └──Expr: Assign
                                 └──Type expr: Int
                                 └──Expr: Variable: _var_i0
                                    └──Type expr: Int
                                 └──Expr: Bin Op: +
                                    └──Type expr: Int
                                    └──Expr: Variable: _var_i0
                                       └──Type expr: Int
                                    └──Expr: Int:1
                     └──Expr: Printf
                        └──Value of y: %d
                        └──Expr: ObjMethod: (Class: _FooInt) _var_y0._getF
                           └── Possible Capabilities:
                              └── Possible Capability: Linear Bar
                           └──Type expr: Int
                           └──()
         └── Method: _setFi
            └── Return type: Void
            └──Param: f
               └──Type expr: Int
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Assign
                  └──Type expr: Int
                  └──Expr: Objfield: (Class: _FooInt) this.f
                     └──Type expr: Int
                     └──Capabilities:
                        └──Capability: Linear Bar
                  └──Expr: Variable: f
                     └──Type expr: Int
         └── Method: _getF
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: _FooInt) this.f
                  └──Type expr: Int
                  └──Capabilities:
                     └──Capability: Linear Bar
         └── Method: _baz
            └── Return type: Void
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Bool
               └──Expr: Let var: _var_z0
                  └──Type expr: _FooBool
                  └──Expr: Function App
                     └──Type expr: _FooBool
                     └──Function: getTrueFoo
                     └──()
               └──Expr: If
                  └──Type expr: Bool
                  └──Expr: Unary Op: !
                     └──Type expr: Bool
                     └──Expr: Objfield: (Class: _FooBool) _var_z0.f
                        └──Type expr: Bool
                        └──Capabilities:
                           └──Capability: Linear Bar
                  └──Then block
                     └──Type expr: Bool
                     └──Expr: ObjMethod: (Class: _FooBool) _var_z0._setFb
                        └── Possible Capabilities:
                           └── Possible Capability: Linear Bar
                        └──Type expr: Bool
                        └──Expr: Bool:true
                  └──Else block
                     └──Type expr: Bool
                     └──Expr: ObjMethod: (Class: _FooBool) _var_z0._setFb
                        └── Possible Capabilities:
                           └── Possible Capability: Linear Bar
                        └──Type expr: Bool
                        └──Expr: Bool:false
      └──Class: _FooInt
         └──Capabilities:
            └──Capability: Linear Bar
         └──Field Defn: f
            └──Modifier: Var
            └──Type expr: Int
            └──Capabilities: Bar
         └── Method: _copy7_FooInt
            └── Return type: Void
            └──Param: x
               └──Type expr: _FooInt
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Void
               └──Expr: Finish_async
                  └──Type expr: Void
                     └── Async Expr Free Vars:
                        └── (_FooInt) this, Capabilities: Bar
                        └── (_FooInt) x, Capabilities: Bar
                     └──Async Expr block
                        └──Type expr: Int
                        └──Expr: Assign
                           └──Type expr: Int
                           └──Expr: Objfield: (Class: _FooInt) this.f
                              └──Type expr: Int
                              └──Capabilities:
                                 └──Capability: Linear Bar
                           └──Expr: Objfield: (Class: _FooInt) x.f
                              └──Type expr: Int
                              └──Capabilities:
                                 └──Capability: Linear Bar
               └── Current ThreadLocal Expr Free Vars:
                  └──Current thread block
                     └──Type expr: Void
                     └──Expr: Let var: _var_y0
                        └──Type expr: _FooInt
                        └──Expr: Constructor for: _FooInt
                           └──Type expr: _FooInt
                           └── Field: f
                              └──Type expr: Int
                              └──Expr: Int:0
                        └──Expr: Let var: _var_i0
                           └──Type expr: Int
                           └──Expr: Int:0
                        └──Expr: While
                           └──Type expr: Void
                           └──Expr: Bin Op: <
                              └──Type expr: Bool
                              └──Expr: Variable: _var_i0
                                 └──Type expr: Int
                              └──Expr: Int:100
                           └──Body block
                              └──Type expr: Int
                              └──Expr: ObjMethod: (Class: _FooInt) _var_y0._setFi
                                 └── Possible Capabilities:
                                    └── Possible Capability: Linear Bar
                                 └──Type expr: Int
                                 └──Expr: Bin Op: +
                                    └──Type expr: Int
                                    └──Expr: ObjMethod: (Class: _FooInt) _var_y0._getF
                                       └── Possible Capabilities:
                                          └── Possible Capability: Linear Bar
                                       └──Type expr: Int
                                       └──()
                                    └──Expr: Variable: _var_i0
                                       └──Type expr: Int
                              └──Expr: Assign
                                 └──Type expr: Int
                                 └──Expr: Variable: _var_i0
                                    └──Type expr: Int
                                 └──Expr: Bin Op: +
                                    └──Type expr: Int
                                    └──Expr: Variable: _var_i0
                                       └──Type expr: Int
                                    └──Expr: Int:1
                     └──Expr: Printf
                        └──Value of y: %d
                        └──Expr: ObjMethod: (Class: _FooInt) _var_y0._getF
                           └── Possible Capabilities:
                              └── Possible Capability: Linear Bar
                           └──Type expr: Int
                           └──()
         └── Method: _setFi
            └── Return type: Void
            └──Param: f
               └──Type expr: Int
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Assign
                  └──Type expr: Int
                  └──Expr: Objfield: (Class: _FooInt) this.f
                     └──Type expr: Int
                     └──Capabilities:
                        └──Capability: Linear Bar
                  └──Expr: Variable: f
                     └──Type expr: Int
         └── Method: _getF
            └── Return type: Int
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Int
               └──Expr: Objfield: (Class: _FooInt) this.f
                  └──Type expr: Int
                  └──Capabilities:
                     └──Capability: Linear Bar
         └── Method: _baz
            └── Return type: Void
            └──Param: Void
            └── Used capabilities
            └──   Capabilities: Bar
            └──Body block
               └──Type expr: Bool
               └──Expr: Let var: _var_z0
                  └──Type expr: _FooBool
                  └──Expr: Function App
                     └──Type expr: _FooBool
                     └──Function: getTrueFoo
                     └──()
               └──Expr: If
                  └──Type expr: Bool
                  └──Expr: Unary Op: !
                     └──Type expr: Bool
                     └──Expr: Objfield: (Class: _FooBool) _var_z0.f
                        └──Type expr: Bool
                        └──Capabilities:
                           └──Capability: Linear Bar
                  └──Then block
                     └──Type expr: Bool
                     └──Expr: ObjMethod: (Class: _FooBool) _var_z0._setFb
                        └── Possible Capabilities:
                           └── Possible Capability: Linear Bar
                        └──Type expr: Bool
                        └──Expr: Bool:true
                  └──Else block
                     └──Type expr: Bool
                     └──Expr: ObjMethod: (Class: _FooBool) _var_z0._setFb
                        └── Possible Capabilities:
                           └── Possible Capability: Linear Bar
                        └──Type expr: Bool
                        └──Expr: Bool:false
      └── Function: getTrueFoo
         └── Return type: _FooBool
         └──Param: Void
         └──Body block
            └──Type expr: _FooBool
            └──Expr: Constructor for: _FooBool
               └──Type expr: _FooBool
               └── Field: f
                  └──Type expr: Bool
                  └──Expr: Bool:true
      └──Main block
         └──Type expr: Int
         └──Expr: Let var: _var_x0
            └──Type expr: _FooInt
            └──Expr: Constructor for: _FooInt
               └──Type expr: _FooInt
               └── Field: f
                  └──Type expr: Int
                  └──Expr: Int:5
         └──Expr: Let var: _var_y0
            └──Type expr: _FooInt
            └──Expr: Constructor for: _FooInt
               └──Type expr: _FooInt
         └──Expr: ObjMethod: (Class: _FooInt) _var_y0._copy7_FooInt
            └── Possible Capabilities:
               └── Possible Capability: Linear Bar
            └──Type expr: Int
            └──Expr: Consume
               └──Expr: Variable: _var_x0
                  └──Type expr: _FooInt
                  └── Possible Capabilities:
                     └── Possible Capability: Linear Bar |}]
