open Ast.Ast_types
open Parsed_ast
open Ast.Pprint_ast
open Core

let indent_space = "   "

let rec pprint_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
  let new_indent = indent_space ^ indent in
  match expr with
  | Integer (_, i) -> print_expr (Fmt.str "Int:%d" i)
  | Boolean (_, b) -> print_expr (Fmt.str "Bool:%b" b)
  | Identifier (_, id) -> (
    match id with
    | Variable var_name ->
        print_expr (Fmt.str "Variable: %s" (Var_name.to_string var_name))
    | ObjField (var_name, field_name) ->
        print_expr
          (Fmt.str "Objfield: %s.%s" (Var_name.to_string var_name)
             (Field_name.to_string field_name)) )
  | Constructor (_, class_name, constructor_args) ->
      print_expr (Fmt.str "Constructor for: %s" (Class_name.to_string class_name)) ;
      List.iter ~f:(pprint_constructor_arg ppf ~indent:new_indent) constructor_args
  | Let (_, var_name, bound_expr) ->
      print_expr (Fmt.str "Let var: %s" (Var_name.to_string var_name)) ;
      pprint_expr ppf ~indent:new_indent bound_expr
  | Assign (loc, id, assigned_expr) ->
      print_expr "Assign" ;
      pprint_expr ppf ~indent:new_indent (Identifier (loc, id)) ;
      pprint_expr ppf ~indent:new_indent assigned_expr
  | Consume (loc, id) ->
      print_expr "Consume" ;
      pprint_expr ppf ~indent:new_indent (Identifier (loc, id))
  | FunctionApp (_, func_name, args) ->
      print_expr "Function App" ;
      Fmt.pf ppf "%sFunction: %s@." new_indent (Function_name.to_string func_name) ;
      pprint_args ppf ~indent:new_indent args
  | Printf (_, format_str, args) ->
      print_expr "Printf" ;
      Fmt.pf ppf "%s%s@." new_indent format_str ;
      pprint_args ppf ~indent:new_indent args
  | MethodApp (_, var_name, method_name, args) ->
      print_expr
        (Fmt.str "ObjMethod: %s.%s" (Var_name.to_string var_name)
           (Method_name.to_string method_name)) ;
      pprint_args ppf ~indent:new_indent args
  | FinishAsync (_, async_exprs, curr_thread_expr) ->
      print_expr "Finish async" ;
      List.iter ~f:(pprint_async_expr ppf ~indent:(indent_space ^ new_indent)) async_exprs ;
      pprint_block_expr ppf ~indent:new_indent ~block_name:"Current thread"
        curr_thread_expr
  | If (_, cond_expr, then_expr, else_expr) ->
      print_expr "If" ;
      pprint_expr ppf ~indent:new_indent cond_expr ;
      pprint_block_expr ppf ~indent:new_indent ~block_name:"Then" then_expr ;
      pprint_block_expr ppf ~indent:new_indent ~block_name:"Else" else_expr
  | While (_, cond_expr, loop_expr) ->
      print_expr "While" ;
      pprint_expr ppf ~indent:new_indent cond_expr ;
      pprint_block_expr ppf ~indent:new_indent ~block_name:"Body" loop_expr
  | For (_, start_expr, cond_expr, step_expr, loop_expr) ->
      print_expr "For" ;
      pprint_expr ppf ~indent:new_indent start_expr ;
      pprint_expr ppf ~indent:new_indent cond_expr ;
      pprint_expr ppf ~indent:new_indent step_expr ;
      pprint_block_expr ppf ~indent:new_indent ~block_name:"Body" loop_expr
  | BinOp (_, bin_op, expr1, expr2) ->
      print_expr (Fmt.str "Bin Op: %s" (string_of_bin_op bin_op)) ;
      pprint_expr ppf ~indent:new_indent expr1 ;
      pprint_expr ppf ~indent:new_indent expr2
  | UnOp (_, un_op, expr) ->
      print_expr (Fmt.str "Unary Op: %s" (string_of_un_op un_op)) ;
      pprint_expr ppf ~indent:new_indent expr

and pprint_constructor_arg ppf ~indent (ConstructorArg (field_name, expr)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s Field: %s@." indent (Field_name.to_string field_name) ;
  pprint_expr ppf ~indent:new_indent expr

and pprint_args ppf ~indent = function
  | []   -> Fmt.pf ppf "%s()@." indent
  | args -> List.iter ~f:(pprint_expr ppf ~indent) args

and pprint_block_expr ppf ~indent ~block_name (Block (_, exprs)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s%s block@." indent block_name ;
  List.iter ~f:(pprint_expr ppf ~indent:new_indent) exprs

and pprint_async_expr ppf ~indent (AsyncExpr block_expr) =
  pprint_block_expr ppf ~indent ~block_name:"Async Expr" block_expr

let pprint_function_defn ppf ~indent
    (TFunction (func_name, return_type, params, body_expr)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s Function: %s@." indent (Function_name.to_string func_name) ;
  Fmt.pf ppf "%s Return type: %s@." new_indent (string_of_type return_type) ;
  pprint_params ppf ~indent:new_indent params ;
  pprint_block_expr ppf ~indent:new_indent ~block_name:"Body" body_expr

let pprint_method_defn ppf ~indent
    (TMethod (method_name, return_type, params, capabilities_used, body_expr)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s Method: %s@." indent (Method_name.to_string method_name) ;
  Fmt.pf ppf "%s Return type: %s@." new_indent (string_of_type return_type) ;
  pprint_params ppf ~indent:new_indent params ;
  Fmt.pf ppf "%s Used capabilities@." new_indent ;
  pprint_capability_names ppf ~indent:(new_indent ^ indent_space) capabilities_used ;
  pprint_block_expr ppf ~indent:new_indent ~block_name:"Body" body_expr

let pprint_class_defn ppf ~indent
    (TClass (class_name, capabilities, field_defns, method_defns)) =
  Fmt.pf ppf "%sClass: %s@." indent (Class_name.to_string class_name) ;
  let new_indent = indent_space ^ indent in
  pprint_capabilities ppf ~indent:new_indent capabilities ;
  List.iter ~f:(pprint_field_defn ppf ~indent:new_indent) field_defns ;
  List.iter ~f:(pprint_method_defn ppf ~indent:new_indent) method_defns

let pprint_program ppf (Prog (class_defns, function_defns, main_expr)) =
  Fmt.pf ppf "Program@." ;
  let indent = "└──" in
  List.iter ~f:(pprint_class_defn ppf ~indent) class_defns ;
  List.iter ~f:(pprint_function_defn ppf ~indent) function_defns ;
  pprint_block_expr ppf ~indent ~block_name:"Main" main_expr
