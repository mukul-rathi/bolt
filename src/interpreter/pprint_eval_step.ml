open Runtime_env
open Core
open Ast_types

let indent_space = "   "

let string_of_value = function
  | NULL         -> "NULL"
  | ADDR address -> Fmt.str "Address: %s" (string_of_address address)
  | INT i        -> Fmt.str "Int: %d" i
  | FUN _        -> "Function(...)"

let rec string_of_expr = function
  | Value v              -> string_of_value v
  | Blocked thread_id    -> Fmt.str "Blocked on thread %s" (string_of_thread_id thread_id)
  | Seq exprs            -> String.concat ~sep:";" (List.map ~f:string_of_expr exprs)
  | TypedExpr typed_expr -> (
    match typed_expr with
    | Integer (_, i)                                                            -> Int
                                                                        .to_string i
    | Variable (_, _, var_name)                                                 -> Var_name
                                                                        .to_string
                                                                                     var_name
    | Lambda (_, _, arg_var, arg_type, body)                                    ->
        Fmt.str "Fun (%s:%s) -> (%s)" (Var_name.to_string arg_var)
          (string_of_type arg_type)
          (string_of_expr (TypedExpr body))
    | App (_, _, func, arg)                                                     ->
        Fmt.str "(%s) (%s)"
          (string_of_expr (TypedExpr func))
          (string_of_expr (TypedExpr arg))
    | Seq (_, _, typed_exprs)                                                   ->
        Fmt.str "Begin: %s :End"
          (String.concat ~sep:";"
             (List.map
                ~f:(fun typed_expr -> string_of_expr (TypedExpr typed_expr))
                typed_exprs))
    | Let (_, _, var_name, typed_expr_to_sub, body_typed_expr)                  ->
        Fmt.str "Let %s = %s in %s end" (Var_name.to_string var_name)
          (string_of_expr (TypedExpr typed_expr_to_sub))
          (string_of_expr (TypedExpr body_typed_expr))
    | ObjField (_, _, var_name, _, field_name)                                  ->
        Fmt.str "%s.%s" (Var_name.to_string var_name) (Field_name.to_string field_name)
    | Assign (_, _, var_name, _, field_name, assigned_typed_expr)               ->
        Fmt.str "%s.%s := %s" (Var_name.to_string var_name)
          (Field_name.to_string field_name)
          (string_of_expr (TypedExpr assigned_typed_expr))
    | Constructor (_, _, class_name, constructor_args)                          ->
        Fmt.str "new %s(%s)"
          (Class_name.to_string class_name)
          (String.concat ~sep:","
             (List.map ~f:string_of_constructor_arg constructor_args))
    | Consume (_, _, typed_expr)                                                ->
        Fmt.str "consume %s" (string_of_expr (TypedExpr typed_expr))
    | FinishAsync (_, _, async_typed_expr1, async_typed_expr2, next_typed_expr) ->
        Fmt.str "finish{ async{ %s } async { %s } } ; %s "
          (string_of_expr (TypedExpr async_typed_expr1))
          (string_of_expr (TypedExpr async_typed_expr2))
          (string_of_expr (TypedExpr next_typed_expr)) )

and string_of_constructor_arg (Typed_ast.ConstructorArg (_, field_name, typed_expr)) =
  Fmt.str "%s: %s"
    (Field_name.to_string field_name)
    (string_of_expr (TypedExpr typed_expr))

let string_of_obj obj =
  let string_of_fields fields =
    String.concat ~sep:";"
      (List.map
         ~f:(fun (field_name, value) ->
           Fmt.str "%s: %s" (Field_name.to_string field_name) (string_of_value value))
         fields) in
  Fmt.str "{ Class_name: %s, Fields: { %s } }"
    (Class_name.to_string obj.class_name)
    (string_of_fields obj.fields)

let string_of_heap heap =
  String.concat ~sep:","
    (List.map
       ~f:(fun (addr, obj) ->
         Fmt.str "%s |-> %s" (string_of_address addr) (string_of_obj obj))
       heap)

let string_of_stack stack =
  String.concat ~sep:","
    (List.map
       ~f:(fun (var_name, value) ->
         Fmt.str "%s |-> %s" (Var_name.to_string var_name) (string_of_value value))
       stack)

let pprint_thread ppf indent (TThread (thread_id, expr, stack)) =
  let new_indent = indent_space ^ indent in
  Fmt.pf ppf "%sThread: %s@." indent (string_of_thread_id thread_id) ;
  Fmt.pf ppf "%sTypedExpr: [ %s ]@." new_indent (string_of_expr expr) ;
  Fmt.pf ppf "%sStack: [ %s ]@." new_indent (string_of_stack stack)

let pprint_eval_step ppf ~step_number thread_pool heap scheduled_thread_id =
  let indent = "└──" in
  Fmt.pf ppf "----- Step %d - scheduled thread : %s-----@." step_number
    (string_of_thread scheduled_thread_id) ;
  Fmt.pf ppf "Threads:@." ;
  List.iter ~f:(pprint_thread ppf indent) thread_pool ;
  Fmt.pf ppf "Heap: [ %s ]@." (string_of_heap heap) ;
  Fmt.pf ppf "------------------------------------------@."
