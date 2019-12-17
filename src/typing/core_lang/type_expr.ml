open Ast.Ast_types
open Parsing
open Type_env
open Core
open Result

(* This checks the type of the expression is consistent with the field it's being assigned
   to in the constructor, and annotates it with the type if so *)
let infer_type_constructor_arg class_defn infer_type_expr_fn loc env
    (Parsed_ast.ConstructorArg (field_name, expr)) =
  (* Check class has field and return its type if so *)
  get_class_field field_name class_defn loc
  >>= fun (TField (_, _, field_type)) ->
  (* check if expr being assigned type-checks and get its type*)
  infer_type_expr_fn expr env
  >>= fun (typed_expr, expr_type) ->
  (* check type of expr being assigned to field matches the field's type*)
  if check_type_equality (field_to_expr_type field_type) expr_type then
    Ok (Typed_ast.ConstructorArg (expr_type, field_name, typed_expr))
  else
    Error
      (Error.of_string
         (Fmt.str
            "%s Type mismatch - constructor expected argument of type %s, instead received type %s@."
            (string_of_loc loc)
            (string_of_type (field_to_expr_type field_type))
            (string_of_type expr_type)))

(* Given a parsed expr, we infer its type and return a tuple consisting of the
   type-annotated expr as the first value, and its type as the second value - the latter
   is explicitly returned since it is often used in recursive calls by the caller. *)
let rec infer_type_expr class_defns trait_defns function_defns (expr : Parsed_ast.expr)
    env =
  let infer_type_with_defns = infer_type_expr class_defns trait_defns function_defns in
  (* Partially apply the function for brevity in recursive calls *)
  match expr with
  | Parsed_ast.Integer (loc, i) -> Ok (Typed_ast.Integer (loc, i), TEInt)
  | Parsed_ast.Variable (loc, var_name) ->
      get_var_type var_name env loc
      >>| fun var_type -> (Typed_ast.Variable (loc, var_type, var_name), var_type)
  | Parsed_ast.App (loc, func_name, args_exprs) ->
      get_function_type func_name function_defns loc
      >>= fun (param_types, return_type) ->
      Result.all (List.map ~f:(fun expr -> infer_type_with_defns expr env) args_exprs)
      >>= fun typed_args_exprs_and_types ->
      let typed_args_exprs, args_types = List.unzip typed_args_exprs_and_types in
      if param_types = args_types then
        Ok (Typed_ast.App (loc, return_type, func_name, typed_args_exprs), return_type)
      else
        Error
          (Error.of_string
             (Fmt.str
                "%s Type mismatch - function expected arguments of type %s, instead received type %s@."
                (string_of_loc loc)
                (String.concat ~sep:" * " (List.map ~f:string_of_type param_types))
                (String.concat ~sep:" * " (List.map ~f:string_of_type args_types))))
  | Parsed_ast.Block (loc, (exprs : Parsed_ast.expr list)) -> (
      (* Check all the subexpressions are consistently typed *)
      Result.all (List.map ~f:(fun expr -> infer_type_with_defns expr env) exprs)
      >>= fun typed_exprs_with_types ->
      let typed_exprs =
        List.map ~f:(fun (typed_expr, _) -> typed_expr) typed_exprs_with_types in
      match List.last typed_exprs_with_types with
      (* Set the type of the expression to be that of the last subexpr in the block *)
      | Some (_, expr_type) ->
          Ok (Typed_ast.Block (loc, expr_type, typed_exprs), expr_type)
      | None ->
          Error
            (Error.of_string
               (Fmt.str "%s Type error - block of expressions is empty@."
                  (string_of_loc loc))) )
  | Parsed_ast.Let (loc, var_name, expr_to_sub, body_expr) ->
      (* Infer type of expression that is being subbed and bind it to the let var then
         type-check the body expr*)
      infer_type_with_defns expr_to_sub env
      >>= fun (typed_expr_to_sub, expr_to_sub_type) ->
      infer_type_with_defns body_expr ((var_name, expr_to_sub_type) :: env)
      >>| fun (typed_body_expr, body_type) ->
      ( Typed_ast.Let (loc, body_type, var_name, typed_expr_to_sub, typed_body_expr)
      , body_type )
  | Parsed_ast.ObjField (loc, var_name, field_name) ->
      (* Get the class definition to determine type of the field. *)
      get_var_type var_name env loc
      >>= fun obj_type ->
      get_obj_class_defn var_name env class_defns loc
      >>= fun class_defn ->
      get_class_field field_name class_defn loc
      >>| fun (TField (_, _, field_type)) ->
      let field_expr_type = field_to_expr_type field_type in
      (* Convert to corresponding expr type to match the type declaration *)
      ( Typed_ast.ObjField (loc, field_expr_type, var_name, obj_type, field_name)
      , field_expr_type )
  | Parsed_ast.Assign (loc, var_name, field_name, assigned_expr) ->
      get_var_type var_name env loc
      >>= fun obj_type ->
      get_obj_class_defn var_name env class_defns loc
      >>= fun class_defn ->
      get_class_field field_name class_defn loc
      >>= fun (TField (mode, _, field_type)) ->
      if mode = MConst then
        Error
          (Error.of_string
             (Fmt.str "%s Type error - Assigning expr to a const field.@."
                (string_of_loc loc)))
      else
        let field_expr_type = field_to_expr_type field_type in
        (* Infer type of assigned expr and check consistent with field type*)
        infer_type_with_defns assigned_expr env
        >>= fun (typed_assigned_expr, assigned_expr_type) ->
        if check_type_equality field_expr_type assigned_expr_type then
          Ok
            ( Typed_ast.Assign
                ( loc
                , assigned_expr_type
                , var_name
                , obj_type
                , field_name
                , typed_assigned_expr )
            , assigned_expr_type )
        else
          Error
            (Error.of_string
               (Fmt.str "%s Type error - Assigning type %s to a field of type %s@."
                  (string_of_loc loc)
                  (string_of_type assigned_expr_type)
                  (string_of_type field_expr_type)))
  | Parsed_ast.Constructor (loc, class_name, constructor_args) ->
      (* Check that there is a matching class defn for the class name provided *)
      get_class_defn class_name class_defns loc
      >>= fun class_defn ->
      (* Check that all the constructor arguments type-check *)
      Result.all
        (List.map
           ~f:(infer_type_constructor_arg class_defn infer_type_with_defns loc env)
           constructor_args)
      >>| fun typed_constructor_args ->
      ( Typed_ast.Constructor (loc, TEClass class_name, class_name, typed_constructor_args)
      , TEClass class_name )
  | Parsed_ast.Consume (loc, expr) ->
      infer_type_with_defns expr env
      >>| fun (typed_expr, expr_type) ->
      (Typed_ast.Consume (loc, expr_type, typed_expr), expr_type)
  | Parsed_ast.FinishAsync (loc, async_expr1, async_expr2, next_expr) ->
      (* Check async expressions type-check - note they have access to same env, as not
         being checked for data races in this stage of type-checking *)
      infer_type_with_defns async_expr1 env
      >>= fun (typed_async_expr1, _) ->
      infer_type_with_defns async_expr2 env
      >>= fun (typed_async_expr2, _) ->
      infer_type_with_defns next_expr env
      >>| fun (typed_next_expr, next_expr_type) ->
      ( Typed_ast.FinishAsync
          (loc, next_expr_type, typed_async_expr1, typed_async_expr2, typed_next_expr)
      , next_expr_type )

(* Top level statement to infer type of overall program expr *)
let type_expr class_defns trait_defns function_defns (expr : Parsed_ast.expr) =
  infer_type_expr class_defns trait_defns function_defns
    (expr : Parsed_ast.expr)
    ([] : type_env)
  >>| fun (typed_expr, _expr_type) -> typed_expr
