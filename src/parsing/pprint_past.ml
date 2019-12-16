open Ast.Ast_types
open Parsed_ast
open Ast.Pprint_ast

let indent_space = "   "

let rec pprint_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
  let new_indent = indent_space ^ indent in
  match expr with
  | Integer (_, i) -> print_expr (Fmt.str "Int:%d" i)
  | Variable (_, var_name) ->
      print_expr (Fmt.str "Variable: %s" (Var_name.to_string var_name))
  | Lambda (_, arg, arg_type, body) ->
      print_expr (Fmt.str "Fun arg: %s" (Var_name.to_string arg)) ;
      pprint_type_expr ppf ~indent:new_indent arg_type ;
      pprint_expr ppf ~indent:new_indent body
  | App (_, func, arg) ->
      print_expr "App" ;
      pprint_expr ppf ~indent:new_indent func ;
      pprint_expr ppf ~indent:new_indent arg
  | Block (_, exprs) ->
      print_expr "Block" ;
      List.iter ~f:(pprint_expr ppf ~indent:new_indent) exprs
  | Let (_, var_name, expr_to_sub, body_expr) ->
      print_expr (Fmt.str "Let var: %s" (Var_name.to_string var_name)) ;
      pprint_expr ppf ~indent:new_indent expr_to_sub ;
      pprint_expr ppf ~indent:new_indent body_expr
  | ObjField (_, var_name, field_name) ->
      print_expr
        (Fmt.str "Objfield: %s.%s" (Var_name.to_string var_name)
           (Field_name.to_string field_name))
  | Assign (_, var_name, field_name, assigned_expr) ->
      print_expr
        (Fmt.str "Assign: %s.%s" (Var_name.to_string var_name)
           (Field_name.to_string field_name)) ;
      pprint_expr ppf ~indent:new_indent assigned_expr
  | Constructor (_, class_name, constructor_args) ->
      print_expr (Fmt.str "Constructor for: %s" (Class_name.to_string class_name)) ;
      List.iter ~f:(pprint_constructor_arg ppf ~indent:new_indent) constructor_args
  | Consume (_, expr) ->
      print_expr "Consume" ;
      pprint_expr ppf ~indent:new_indent expr
  | FinishAsync (_, async_expr1, async_expr2, next_expr) ->
      print_expr "Finish_async" ;
      pprint_expr ppf ~indent:new_indent async_expr1 ;
      pprint_expr ppf ~indent:new_indent async_expr2 ;
      pprint_expr ppf ~indent:new_indent next_expr

and pprint_constructor_arg ppf ~indent (ConstructorArg (field_name, expr)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s Field: %s@." indent (Field_name.to_string field_name) ;
  pprint_expr ppf ~indent:new_indent expr

let pprint_program ppf (Prog (class_defns, trait_defns, expr)) =
  Fmt.pf ppf "Program@." ;
  let indent = "└──" in
  List.iter ~f:(pprint_class_defn ppf ~indent) class_defns ;
  List.iter ~f:(pprint_trait_defn ppf ~indent) trait_defns ;
  pprint_expr ppf ~indent expr
