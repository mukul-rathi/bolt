open Ast_types
open Typed_ast
open Pprint_ast

let indent_space = "   "

let pprint_lambda_arg ppf indent arg_var arg_type =
  let print_expr = Fmt.pf ppf "%sArg: %s@." indent in
  let new_indent = indent_space ^ indent in
  print_expr (Var_name.to_string arg_var) ;
  pprint_type_expr ppf new_indent arg_type

let rec pprint_expr ppf indent expr =
  let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
  let new_indent = indent_space ^ indent in
  match expr with
  | Integer i                                                    -> print_expr
                                                                      ( "Int:"
                                                                      ^ string_of_int i
                                                                      )
  | Variable (type_expr, var_name)                               ->
      print_expr ("Variable:" ^ Var_name.to_string var_name) ;
      pprint_type_expr ppf new_indent type_expr
  | Lambda (type_expr, arg_var, arg_type, body)                  ->
      print_expr ("Lambda fun: " ^ Var_name.to_string arg_var) ;
      pprint_type_expr ppf new_indent type_expr ;
      pprint_lambda_arg ppf new_indent arg_var arg_type ;
      pprint_expr ppf new_indent body
  | App (type_expr, func, arg)                                   ->
      print_expr "App" ;
      pprint_type_expr ppf new_indent type_expr ;
      pprint_expr ppf new_indent func ;
      pprint_expr ppf new_indent arg
  | Seq (type_expr, exprs)                                       ->
      print_expr "Seq" ;
      pprint_type_expr ppf new_indent type_expr ;
      List.iter (pprint_expr ppf new_indent) exprs
  | Let (type_expr, var_name, expr_to_sub, body_expr)            ->
      print_expr ("Let var: " ^ Var_name.to_string var_name) ;
      pprint_type_expr ppf new_indent type_expr ;
      pprint_expr ppf new_indent expr_to_sub ;
      pprint_expr ppf new_indent body_expr
  | ObjField (type_expr, var_name, field_name)                   ->
      print_expr
        ( "Objfield: " ^ Var_name.to_string var_name ^ "."
        ^ Field_name.to_string field_name ) ;
      pprint_type_expr ppf new_indent type_expr
  | Assign (type_expr, var_name, field_name, assigned_expr)      ->
      print_expr
        ("Assign: " ^ Var_name.to_string var_name ^ "." ^ Field_name.to_string field_name) ;
      pprint_type_expr ppf new_indent type_expr ;
      pprint_expr ppf new_indent assigned_expr
  | Constructor (type_expr, class_name, constructor_args)        ->
      print_expr ("Constructor for:" ^ Class_name.to_string class_name) ;
      pprint_type_expr ppf new_indent type_expr ;
      List.iter (pprint_constructor_arg ppf new_indent) constructor_args
  | Consume (type_expr, expr)                                    ->
      print_expr "Consume" ;
      pprint_type_expr ppf new_indent type_expr ;
      pprint_expr ppf new_indent expr
  | FinishAsync (type_expr, async_expr1, async_expr2, next_expr) ->
      print_expr "Finish_async" ;
      pprint_type_expr ppf new_indent type_expr ;
      pprint_expr ppf new_indent async_expr1 ;
      pprint_expr ppf new_indent async_expr2 ;
      pprint_expr ppf new_indent next_expr

and pprint_constructor_arg ppf indent (ConstructorArg (type_expr, field_name, expr)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s Field: %s@." indent (Field_name.to_string field_name) ;
  pprint_type_expr ppf new_indent type_expr ;
  pprint_expr ppf new_indent expr

let pprint_program ppf (Prog (class_defns, trait_defns, expr)) =
  Fmt.pf ppf "Program@." ;
  let indent = "└──" in
  List.iter (pprint_class_defn ppf indent) class_defns ;
  List.iter (pprint_trait_defn ppf indent) trait_defns ;
  pprint_expr ppf indent expr
