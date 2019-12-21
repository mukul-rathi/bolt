open Ast.Ast_types
open Typed_ast
open Ast.Pprint_ast
open Core

let indent_space = "   "

let rec pprint_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
  let new_indent = indent_space ^ indent in
  match expr with
  | Unit _ -> print_expr "()"
  | Integer (_, i) -> print_expr (Fmt.str "Int:%d" i)
  | Boolean (_, b) -> print_expr (Fmt.str "Bool:%b" b)
  | Variable (_, type_expr, var_name) ->
      print_expr (Fmt.str "Variable: %s" (Var_name.to_string var_name)) ;
      pprint_type_expr ppf ~indent:new_indent type_expr
  | App (_, type_expr, func_name, args) ->
      print_expr "App" ;
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      Fmt.pf ppf "%sFunction: %s@." new_indent (Function_name.to_string func_name) ;
      List.iter ~f:(pprint_expr ppf ~indent:new_indent) args
  | Block (_, type_expr, exprs) ->
      print_expr "Block" ;
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      List.iter ~f:(pprint_expr ppf ~indent:new_indent) exprs
  | Let (_, type_expr, var_name, bound_expr) ->
      print_expr (Fmt.str "Let var: %s" (Var_name.to_string var_name)) ;
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      pprint_expr ppf ~indent:new_indent bound_expr
  | ObjField (_, type_expr, var_name, obj_type, field_name) ->
      print_expr
        (Fmt.str "Objfield: (%s) %s.%s" (string_of_type obj_type)
           (Var_name.to_string var_name)
           (Field_name.to_string field_name)) ;
      pprint_type_expr ppf ~indent:new_indent type_expr
  | ObjMethod (_, type_expr, var_name, obj_type, method_name, args) ->
      print_expr
        (Fmt.str "ObjMethod: (%s) %s.%s" (string_of_type obj_type)
           (Var_name.to_string var_name)
           (Function_name.to_string method_name)) ;
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      List.iter ~f:(pprint_expr ppf ~indent:new_indent) args
  | Assign (_, type_expr, var_name, obj_type, field_name, assigned_expr) ->
      print_expr
        (Fmt.str "Assign: (%s) %s.%s" (string_of_type obj_type)
           (Var_name.to_string var_name)
           (Field_name.to_string field_name)) ;
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      pprint_expr ppf ~indent:new_indent assigned_expr
  | Constructor (_, type_expr, class_name, constructor_args) ->
      print_expr (Fmt.str "Constructor for: %s" (Class_name.to_string class_name)) ;
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      List.iter ~f:(pprint_constructor_arg ppf new_indent) constructor_args
  | Consume (_, type_expr, expr) ->
      print_expr "Consume" ;
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      pprint_expr ppf ~indent:new_indent expr
  | FinishAsync (_, type_expr, async_expr1, async_expr2, next_expr) ->
      print_expr "Finish_async" ;
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      pprint_expr ppf ~indent:new_indent async_expr1 ;
      pprint_expr ppf ~indent:new_indent async_expr2 ;
      pprint_expr ppf ~indent:new_indent next_expr
  | If (_, type_expr, cond_expr, then_expr, else_expr) ->
      print_expr "Finish_async" ;
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      pprint_expr ppf ~indent:new_indent cond_expr ;
      pprint_expr ppf ~indent:new_indent then_expr ;
      pprint_expr ppf ~indent:new_indent else_expr
  | BinOp (_, type_expr, bin_op, expr1, expr2) ->
      print_expr (Fmt.str "Bin Op: %s" (string_of_bin_op bin_op)) ;
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      pprint_expr ppf ~indent:new_indent expr1 ;
      pprint_expr ppf ~indent:new_indent expr2
  | UnOp (_, type_expr, un_op, expr) ->
      print_expr (Fmt.str "Unary Op: %s" (string_of_un_op un_op)) ;
      pprint_type_expr ppf ~indent:new_indent type_expr ;
      pprint_expr ppf ~indent:new_indent expr

and pprint_constructor_arg ppf indent (ConstructorArg (type_expr, field_name, expr)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s Field: %s@." indent (Field_name.to_string field_name) ;
  pprint_type_expr ppf ~indent:new_indent type_expr ;
  pprint_expr ppf ~indent:new_indent expr

let pprint_function_defn ppf ~indent
    (TFunction (func_name, return_type, params, body_expr)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s Function: %s@." indent (Function_name.to_string func_name) ;
  Fmt.pf ppf "%s Return type: %s@." new_indent (string_of_type return_type) ;
  List.iter ~f:(pprint_param ppf ~indent:new_indent) params ;
  pprint_expr ppf ~indent:new_indent body_expr

let pprint_class_defn ppf ~indent
    (TClass (class_name, cap_trait, field_defns, method_defns)) =
  Fmt.pf ppf "%sClass: %s@." indent (Class_name.to_string class_name) ;
  let new_indent = indent_space ^ indent in
  pprint_cap_trait ppf ~indent:new_indent cap_trait ;
  List.iter ~f:(pprint_field_defn ppf ~indent:new_indent) field_defns ;
  List.iter ~f:(pprint_function_defn ppf ~indent:new_indent) method_defns

let pprint_program ppf (Prog (class_defns, trait_defns, function_defns, expr)) =
  Fmt.pf ppf "Program@." ;
  let indent = "└──" in
  List.iter ~f:(pprint_class_defn ppf ~indent) class_defns ;
  List.iter ~f:(pprint_trait_defn ppf ~indent) trait_defns ;
  List.iter ~f:(pprint_function_defn ppf ~indent) function_defns ;
  pprint_expr ppf ~indent expr
