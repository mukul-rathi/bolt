open Core
open Print_parsed_ast

let%expect_test "Good if statement" =
  print_parsed_ast
    " 
   void main() {
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
      └──Main block
         └──Expr: Let var: x
            └──Expr: Bool:true
         └──Expr: If
            └──Expr: Variable: x
            └──Then block
               └──Expr: Int:0
            └──Else block
               └──Expr: Int:1 |}]

let%expect_test "Good while loop" =
  print_parsed_ast " 
  void main(){
   while (1 < 2){
     let x = 5
   }
  }
  " ;
  [%expect
    {|
      Program
      └──Main block
         └──Expr: While
            └──Expr: Bin Op: <
               └──Expr: Int:1
               └──Expr: Int:2
            └──Body block
               └──Expr: Let var: x
                  └──Expr: Int:5 |}]

let%expect_test "Good for loop" =
  print_parsed_ast
    " 
  void main(){
    for (let i = 0; i < (5*5); i := i+1) {
      i
    }
   }
  " ;
  [%expect
    {|
      Program
      └──Main block
         └──Expr: For
            └──Expr: Let var: i
               └──Expr: Int:0
            └──Expr: Bin Op: <
               └──Expr: Variable: i
               └──Expr: Bin Op: *
                  └──Expr: Int:5
                  └──Expr: Int:5
            └──Expr: Assign
               └──Expr: Variable: i
               └──Expr: Bin Op: +
                  └──Expr: Variable: i
                  └──Expr: Int:1
            └──Body block
               └──Expr: Variable: i |}]
