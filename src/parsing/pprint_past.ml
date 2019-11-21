open Ast_types
open Parsed_ast
open Format
open Pprint_ast

let indent_space = "   "

let rec pprint_expr ppf indent expr =
  let print_expr = fprintf ppf "%sExpr: %s@." indent in
  let new_indent = indent_space ^ indent in
  match expr with
  | Integer (_, i)                                       -> print_expr
                                                              ("Int:" ^ string_of_int i)
  | Variable (_, var_name)                               -> print_expr
                                                              ( "Variable:"
                                                              ^ Var_name.to_string
                                                                  var_name )
  | Lambda (_, arg, arg_type, body)                      ->
      print_expr ("Fun arg: " ^ Var_name.to_string arg) ;
      pprint_type_expr ppf new_indent arg_type ;
      pprint_expr ppf new_indent body
  | App (_, func, arg)                                   ->
      print_expr "App" ;
      pprint_expr ppf new_indent func ;
      pprint_expr ppf new_indent arg
  | Seq (_, exprs)                                       ->
      print_expr "Seq" ;
      List.iter (pprint_expr ppf new_indent) exprs
  | Let (_, var_name, expr_to_sub, body_expr)            ->
      print_expr ("Let var: " ^ Var_name.to_string var_name) ;
      pprint_expr ppf new_indent expr_to_sub ;
      pprint_expr ppf new_indent body_expr
  | ObjField (_, var_name, field_name)                   ->
      print_expr
        ( "Objfield: " ^ Var_name.to_string var_name ^ "."
        ^ Field_name.to_string field_name )
  | Assign (_, var_name, field_name, assigned_expr)      ->
      print_expr
        ("Assign: " ^ Var_name.to_string var_name ^ "." ^ Field_name.to_string field_name) ;
      pprint_expr ppf new_indent assigned_expr
  | Constructor (_, class_name, constructor_args)        ->
      print_expr ("Constructor for:" ^ Class_name.to_string class_name) ;
      List.iter (pprint_constructor_arg ppf new_indent) constructor_args
  | Consume (_, var_name)                                ->
      print_expr ("Consume variable:" ^ Var_name.to_string var_name)
  | FinishAsync (_, async_expr1, async_expr2, next_expr) ->
      print_expr "Finish_async" ;
      pprint_expr ppf new_indent async_expr1 ;
      pprint_expr ppf new_indent async_expr2 ;
      pprint_expr ppf new_indent next_expr

and pprint_constructor_arg ppf indent (ConstructorArgs (field_name, expr)) =
  let new_indent = indent_space ^ indent in
  fprintf ppf "%s Field: %s@." indent (Field_name.to_string field_name) ;
  pprint_expr ppf new_indent expr

let pprint_program ppf (Prog (class_defns, trait_defns, expr)) =
  fprintf ppf "Program@." ;
  let indent = "└──" in
  List.iter (pprint_class_defn ppf indent) class_defns ;
  List.iter (pprint_trait_defn ppf indent) trait_defns ;
  pprint_expr ppf indent expr
