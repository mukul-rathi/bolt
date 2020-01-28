open Core
open Print_typed_ast

let%expect_test "Good if statement" =
  print_typed_ast
    " 
   void main(){
     let x = true;
     if x {
       0
     }
     else {
       1
     }
   }
  " ;
  [%expect
    {|
      Program
      └──Expr: Block
         └──Type expr: Int
         └──Expr: Let var: _var_x0
            └──Type expr: Bool
            └──Expr: Bool:true
         └──Expr: If
            └──Type expr: Int
            └──Expr: Variable: _var_x0
               └──Type expr: Bool
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Int:0
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Int:1 |}]

let%expect_test "Good while loop" =
  print_typed_ast " 
  void main(){
   while (1 < 2){
     let x = 5
   }
  }
  " ;
  [%expect
    {|
      Program
      └──Expr: Block
         └──Type expr: Void
         └──Expr: While
            └──Type expr: Void
            └──Expr: Bin Op: <
               └──Type expr: Bool
               └──Expr: Int:1
               └──Expr: Int:2
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Let var: _var_x0
                  └──Type expr: Int
                  └──Expr: Int:5 |}]

let%expect_test "Good for loop" =
  print_typed_ast
    " 
  void main(){
    for (let i=0; i < (5*5); i:= i+1) {
      i
    }
   }
  " ;
  [%expect
    {|
      Program
      └──Expr: Block
         └──Type expr: Void
         └──Expr: For
            └──Type expr: Void
            └──Expr: Let var: _var_i0
               └──Type expr: Int
               └──Expr: Int:0
            └──Expr: Bin Op: <
               └──Type expr: Bool
               └──Expr: Variable: _var_i0
                  └──Type expr: Int
               └──Expr: Bin Op: *
                  └──Type expr: Int
                  └──Expr: Int:5
                  └──Expr: Int:5
            └──Expr: Assign
               └──Type expr: Int
               └──Expr: Variable: _var_i0
                  └──Type expr: Int
               └──Expr: Bin Op: +
                  └──Type expr: Int
                  └──Expr: Variable: _var_i0
                     └──Type expr: Int
                  └──Expr: Int:1
            └──Expr: Block
               └──Type expr: Int
               └──Expr: Variable: _var_i0
                  └──Type expr: Int |}]
