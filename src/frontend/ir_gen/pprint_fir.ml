open Frontend_ir
open Core

let indent_space = "   "

let pprint_param ppf ~indent = function
  | TParam (param_type, param_name) ->
      Fmt.pf ppf "%sParam: %s %s@." indent (string_of_type param_type) param_name

let pprint_params ppf ~indent = function
  | []     -> Fmt.pf ppf "%sParam: %s@." indent (string_of_type TEVoid)
  | params -> List.iter ~f:(pprint_param ppf ~indent) params

let rec pprint_expr ppf ~indent expr =
  let print_expr = Fmt.pf ppf "%sExpr: %s@." indent in
  let new_indent = indent_space ^ indent in
  match expr with
  | Unit -> print_expr "()"
  | Integer i -> print_expr (Fmt.str "Int:%d" i)
  | Boolean b -> print_expr (Fmt.str "Bool:%b" b)
  | Identifier id -> (
    match id with
    | Variable var_name -> print_expr (Fmt.str "Variable: %s" var_name)
    | ObjField (var_name, field_index) ->
        print_expr (Fmt.str "Objfield: %s[%d]" var_name field_index) )
  | Constructor (class_name, constructor_args) ->
      print_expr (Fmt.str "Constructor for: %s" class_name) ;
      List.iter ~f:(pprint_constructor_arg ppf new_indent) constructor_args
  | Let (var_name, bound_expr) ->
      print_expr (Fmt.str "Let var: %s" var_name) ;
      pprint_expr ppf ~indent:new_indent bound_expr
  | Assign (id, assigned_expr) ->
      print_expr "Assign" ;
      pprint_expr ppf ~indent:new_indent (Identifier id) ;
      pprint_expr ppf ~indent:new_indent assigned_expr
  | Consume id ->
      print_expr "Consume" ;
      pprint_expr ppf ~indent:new_indent (Identifier id)
  | FunctionApp (func_name, args) ->
      print_expr "Function App" ;
      Fmt.pf ppf "%sFunction: %s@." new_indent func_name ;
      List.iter ~f:(pprint_expr ppf ~indent:new_indent) args
  | FinishAsync (async_exprs, curr_thread_expr) ->
      print_expr "Finish_async" ;
      List.iter
        ~f:
          (pprint_block_expr ppf ~indent:(indent_space ^ new_indent)
             ~block_name:"Async Expr")
        async_exprs ;
      pprint_block_expr ppf ~indent:new_indent ~block_name:"Current Thread Expr"
        curr_thread_expr
  | IfElse (cond_expr, then_expr, else_expr) ->
      print_expr "If" ;
      pprint_expr ppf ~indent:new_indent cond_expr ;
      pprint_block_expr ppf ~indent:new_indent ~block_name:"Then" then_expr ;
      pprint_block_expr ppf ~indent:new_indent ~block_name:"Else" else_expr
  | WhileLoop (cond_expr, loop_expr) ->
      print_expr "While" ;
      pprint_expr ppf ~indent:new_indent cond_expr ;
      pprint_block_expr ppf ~indent:new_indent ~block_name:"Body" loop_expr
  | BinOp (bin_op, expr1, expr2) ->
      print_expr (Fmt.str "Bin Op: %s" (string_of_bin_op bin_op)) ;
      pprint_expr ppf ~indent:new_indent expr1 ;
      pprint_expr ppf ~indent:new_indent expr2
  | UnOp (un_op, expr) ->
      print_expr (Fmt.str "Unary Op: %s" (string_of_un_op un_op)) ;
      pprint_expr ppf ~indent:new_indent expr

and pprint_constructor_arg ppf indent (ConstructorArg (field_index, expr)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s Field: %d@." indent field_index ;
  pprint_expr ppf ~indent:new_indent expr

and pprint_block_expr ppf ~indent ~block_name exprs =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s%s block@." indent block_name ;
  List.iter ~f:(pprint_expr ppf ~indent:new_indent) exprs

let pprint_function_defn ppf ~indent
    (TFunction (func_name, return_type, params, body_expr)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%s Function: %s@." indent func_name ;
  Fmt.pf ppf "%s Return type: %s@." new_indent (string_of_type return_type) ;
  pprint_params ppf ~indent:new_indent params ;
  pprint_block_expr ppf ~indent:new_indent ~block_name:"Body" body_expr

let pprint_class_defn ppf ~indent (TClass (class_name, field_types)) =
  Fmt.pf ppf "%sClass: %s@." indent class_name ;
  let new_indent = indent_space ^ indent in
  List.iter
    ~f:(fun field_type ->
      Fmt.pf ppf "%sField: %s@." new_indent (string_of_type field_type))
    field_types

let pprint_program ppf (Prog (class_defns, function_defns, exprs)) =
  Fmt.pf ppf "Program@." ;
  let indent = "└──" in
  List.iter ~f:(pprint_class_defn ppf ~indent) class_defns ;
  List.iter ~f:(pprint_function_defn ppf ~indent) function_defns ;
  Fmt.pf ppf "%sMain expr@." indent ;
  List.iter ~f:(pprint_expr ppf ~indent:(indent_space ^ indent)) exprs
