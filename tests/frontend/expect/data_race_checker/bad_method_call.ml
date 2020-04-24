open Core
open Print_data_race_checker_ast

let%expect_test "Access method without all required capabilities" =
  print_data_race_checker_ast
    " 
    class Foo {
      capability linear Bar, local Baz;
      var int f : Bar, Baz;
      int id (int x): Bar { x}
    }
    void main(){
      let x = new Foo(); 
      let y = x; 
      y.f:= x.id(5)
    }
  " ;
  [%expect
    {|
    Line:10 Position:13 Potential data race: _var_x0's method _idi's capability constraints not satisfied. |}]
