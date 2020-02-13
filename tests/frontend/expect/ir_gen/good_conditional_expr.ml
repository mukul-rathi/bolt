open Core
open Print_frontend_ir

let%expect_test "Good if statement" =
  print_frontend_ir
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
      └──Main expr
         └──Expr: Let var: _var_x0
            └──Expr: Bool:true
         └──Expr: If
            └──Expr: Variable: _var_x0
               └──Locked false
            └──Then block
               └──Expr: Int:0
            └──Else block
               └──Expr: Int:1 |}]

let%expect_test "Good while loop" =
  print_frontend_ir " 
  void main(){
   while (1 < 2){
     let x = 5
   }
  }
  " ;
  [%expect
    {|
      Program
      └──Main expr
         └──Expr: While
            └──Expr: Bin Op: <
               └──Expr: Int:1
               └──Expr: Int:2
            └──Body block
               └──Expr: Let var: _var_x0
                  └──Expr: Int:5 |}]

let%expect_test "Good for loop" =
  print_frontend_ir
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
      └──Main expr
            └──Expr: Let var: _var_i0
               └──Expr: Int:0
            └──Expr: While
               └──Expr: Bin Op: <
                  └──Expr: Variable: _var_i0
                     └──Locked false
                  └──Expr: Bin Op: *
                     └──Expr: Int:5
                     └──Expr: Int:5
               └──Body block
                  └──Expr: Variable: _var_i0
                     └──Locked false
                  └──Expr: Assign
                     └──Expr: Variable: _var_i0
                        └──Locked false
                     └──Expr: Bin Op: +
                        └──Expr: Variable: _var_i0
                           └──Locked false
                        └──Expr: Int:1 |}]
