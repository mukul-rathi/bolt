open Ast.Ast_types
open Parsing
open Core_lang_type_env
open Core
open Result

(* This checks the type of the expression is consistent with the field it's being assigned
   to in the constructor, and annotates it with the type if so *)
let infer_type_constructor_arg class_defn infer_type_expr_fn loc env
    (Parsed_ast.ConstructorArg (field_name, expr)) =
  (* Check class has field and return its type if so *)
  get_class_field field_name class_defn loc
  >>= fun (TField (_, field_type, _, _)) ->
  (* check if expr being assigned type-checks and get its type*)
  infer_type_expr_fn expr env
  >>= fun (typed_expr, expr_type) ->
  (* check type of expr being assigned to field matches the field's type*)
  if field_type = expr_type then
    Ok (Typed_ast.ConstructorArg (expr_type, field_name, typed_expr))
  else
    Error
      (Error.of_string
         (Fmt.str
            "%s Type mismatch - constructor expected argument of type %s, instead received type %s@."
            (string_of_loc loc) (string_of_type field_type) (string_of_type expr_type)))

(* Look up in env and class defns to get the types of an identifier *)
let infer_type_identifier class_defns identifier env loc =
  match identifier with
  | Parsed_ast.ObjField (var_name, field_name) ->
      (* Get the class definition to determine type of the field. *)
      get_var_type var_name env loc
      >>= fun obj_type ->
      get_obj_class_defn var_name env class_defns loc
      >>= fun class_defn ->
      get_class_field field_name class_defn loc
      >>| fun (TField (_, field_type, _, _)) ->
      (* Convert to corresponding expr type to match the type declaration *)
      (Typed_ast.ObjField (obj_type, var_name, field_type, field_name), field_type)
  | Parsed_ast.Variable var_name ->
      get_var_type var_name env loc
      >>| fun var_type -> (Typed_ast.Variable (var_type, var_name), var_type)

(* Given a parsed expr, we infer its type and return a tuple consisting of the
   type-annotated expr as the first value, and its type as the second value - the latter
   is explicitly returned since it is often used in recursive calls by the caller. *)
let rec infer_type_expr class_defns function_defns (expr : Parsed_ast.expr) env =
  let infer_type_with_defns = infer_type_expr class_defns function_defns in
  (* Partially apply the function for brevity in recursive calls *)
  match expr with
  | Parsed_ast.Unit loc -> Ok (Typed_ast.Unit loc, TEVoid)
  | Parsed_ast.Integer (loc, i) -> Ok (Typed_ast.Integer (loc, i), TEInt)
  | Parsed_ast.Boolean (loc, b) -> Ok (Typed_ast.Boolean (loc, b), TEBool)
  | Parsed_ast.Identifier (loc, id) ->
      infer_type_identifier class_defns id env loc
      >>| fun (typed_id, id_type) -> (Typed_ast.Identifier (loc, typed_id), id_type)
  | Parsed_ast.Block (loc, (exprs : Parsed_ast.expr list)) -> (
      check_no_var_shadowing_in_block exprs loc
      >>= fun () ->
      match exprs with
      | []                      ->
          Error
            (Error.of_string
               (Fmt.str "%s Type error - block of expressions is empty@."
                  (string_of_loc loc)))
      | [expr]                  ->
          infer_type_with_defns expr env
          >>| fun (typed_expr, expr_type) ->
          (Typed_ast.Block (loc, expr_type, [typed_expr]), expr_type)
      | expr1 :: expr2 :: exprs -> (
          infer_type_with_defns expr1 env
          >>= fun (typed_expr1, expr1_type) ->
          (* Need to update env for subsequent expressions in block with let-binding if
             previous expr was a let-binding *)
          (let updated_env =
             match typed_expr1 with
             | Typed_ast.Let (_, _, var_name, _) -> (var_name, expr1_type) :: env
             | _ -> env in
           infer_type_with_defns (Parsed_ast.Block (loc, expr2 :: exprs)) updated_env)
          >>= fun (typed_block_exprs, block_expr_type) ->
          match typed_block_exprs with
          | Typed_ast.Block (_, _, typed_exprs) ->
              Ok
                ( Typed_ast.Block (loc, block_expr_type, typed_expr1 :: typed_exprs)
                , block_expr_type )
          | _ ->
              Error
                (Error.of_string
                   (Fmt.str "%s Type error - expecting a block of expressions.@."
                      (string_of_loc loc))) ) )
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
  | Parsed_ast.Let (loc, var_name, bound_expr) ->
      (* Infer type of expression that is being subbed and bind it to the let var*)
      infer_type_with_defns bound_expr env
      >>| fun (typed_bound_expr, bound_expr_type) ->
      (Typed_ast.Let (loc, bound_expr_type, var_name, typed_bound_expr), bound_expr_type)
  | Parsed_ast.Assign (loc, id, assigned_expr) ->
      check_identifier_mutable class_defns id env loc
      >>= fun () ->
      infer_type_identifier class_defns id env loc
      >>= fun (typed_id, id_type) ->
      infer_type_with_defns assigned_expr env
      >>= fun (typed_assigned_expr, assigned_expr_type) ->
      if id_type = assigned_expr_type then
        Ok
          ( Typed_ast.Assign (loc, assigned_expr_type, typed_id, typed_assigned_expr)
          , assigned_expr_type )
      else
        Error
          (Error.of_string
             (Fmt.str "%s Type error - Assigning type %s to a field of type %s@."
                (string_of_loc loc)
                (string_of_type assigned_expr_type)
                (string_of_type id_type)))
  | Parsed_ast.Consume (loc, id) ->
      infer_type_identifier class_defns id env loc
      >>| fun (typed_id, id_type) -> (Typed_ast.Consume (loc, typed_id), id_type)
  | Parsed_ast.MethodApp (loc, var_name, method_name, args_exprs) ->
      get_var_type var_name env loc
      >>= fun obj_type ->
      get_obj_class_defn var_name env class_defns loc
      >>= fun class_defn ->
      get_method_type method_name class_defn loc
      >>= fun (param_types, return_type) ->
      Result.all (List.map ~f:(fun expr -> infer_type_with_defns expr env) args_exprs)
      >>= fun typed_args_exprs_and_types ->
      let typed_args_exprs, args_types = List.unzip typed_args_exprs_and_types in
      if param_types = args_types then
        Ok
          ( Typed_ast.MethodApp
              (loc, return_type, var_name, obj_type, method_name, typed_args_exprs)
          , return_type )
      else
        Error
          (Error.of_string
             (Fmt.str
                "%s Type mismatch - function expected arguments of type %s, instead received type %s@."
                (string_of_loc loc)
                (String.concat ~sep:" * " (List.map ~f:string_of_type param_types))
                (String.concat ~sep:" * " (List.map ~f:string_of_type args_types))))
  | Parsed_ast.FunctionApp (loc, func_name, args_exprs) ->
      get_function_type func_name function_defns loc
      >>= fun (param_types, return_type) ->
      Result.all (List.map ~f:(fun expr -> infer_type_with_defns expr env) args_exprs)
      >>= fun typed_args_exprs_and_types ->
      let typed_args_exprs, args_types = List.unzip typed_args_exprs_and_types in
      if param_types = args_types then
        Ok
          ( Typed_ast.FunctionApp (loc, return_type, func_name, typed_args_exprs)
          , return_type )
      else
        Error
          (Error.of_string
             (Fmt.str
                "%s Type mismatch - function expected arguments of type %s, instead received type %s@."
                (string_of_loc loc)
                (String.concat ~sep:" * " (List.map ~f:string_of_type param_types))
                (String.concat ~sep:" * " (List.map ~f:string_of_type args_types))))
  | Parsed_ast.FinishAsync (loc, async_exprs, curr_thread_expr) ->
      (* Check async expressions type-check - note they have access to same env, as not
         being checked for data races in this stage of type-checking *)
      Result.all
        (List.map ~f:(fun async_expr -> infer_type_with_defns async_expr env) async_exprs)
      >>= fun typed_async_exprs_with_types ->
      let typed_async_exprs, _ = List.unzip typed_async_exprs_with_types in
      infer_type_with_defns curr_thread_expr env
      >>| fun (typed_curr_thread_expr, curr_thread_expr_type) ->
      ( Typed_ast.FinishAsync
          (loc, curr_thread_expr_type, typed_async_exprs, typed_curr_thread_expr)
      , curr_thread_expr_type )
  (* We return type of the expr occurring on the current thread, not the forked thread *)
  | Parsed_ast.If (loc, cond_expr, then_expr, else_expr) -> (
      infer_type_with_defns cond_expr env
      >>= fun (typed_cond_expr, cond_expr_type) ->
      infer_type_with_defns then_expr env
      >>= fun (typed_then_expr, then_expr_type) ->
      infer_type_with_defns else_expr env
      >>= fun (typed_else_expr, else_expr_type) ->
      if not (then_expr_type = else_expr_type) then
        Error
          (Error.of_string
             (Fmt.str
                "%s Type error - If statement branches' types' not consistent - then branch has type %s but else branch has type %s@."
                (string_of_loc loc)
                (string_of_type then_expr_type)
                (string_of_type else_expr_type)))
      else
        match cond_expr_type with
        | TEBool ->
            Ok
              ( Typed_ast.If
                  (loc, then_expr_type, typed_cond_expr, typed_then_expr, typed_else_expr)
              , then_expr_type )
        | _      ->
            Error
              (Error.of_string
                 (Fmt.str
                    "%s Type error - If statement condition expression should have boolean type but instead has type %s@."
                    (string_of_loc loc)
                    (string_of_type cond_expr_type))) )
  | Parsed_ast.While (loc, cond_expr, loop_expr) -> (
      infer_type_with_defns cond_expr env
      >>= fun (typed_cond_expr, cond_expr_type) ->
      infer_type_with_defns loop_expr env
      >>= fun (typed_loop_expr, _) ->
      match cond_expr_type with
      | TEBool -> Ok (Typed_ast.While (loc, typed_cond_expr, typed_loop_expr), TEVoid)
      | _      ->
          Error
            (Error.of_string
               (Fmt.str
                  "%s Type error - While loop condition expression should have boolean type but instead has type %s@."
                  (string_of_loc loc)
                  (string_of_type cond_expr_type))) )
  | Parsed_ast.For (loc, start_expr, cond_expr, step_expr, loop_expr) -> (
      (* Type check for loop expressions in context of start expr *)
      infer_type_with_defns (Parsed_ast.Block (loc, [start_expr; cond_expr])) env
      >>= fun (_, cond_expr_type) ->
      ( match cond_expr_type with
      | TEBool -> Ok ()
      | _      ->
          Error
            (Error.of_string
               (Fmt.str
                  "%s Type error - For loop condition expression should have boolean type but instead has type %s@."
                  (string_of_loc loc)
                  (string_of_type cond_expr_type))) )
      >>= fun () ->
      infer_type_with_defns
        (Parsed_ast.Block (loc, [start_expr; cond_expr; loop_expr; step_expr]))
        env
      >>= function
      | ( Typed_ast.Block
            (_, _, [typed_start_expr; typed_cond_expr; typed_loop_expr; typed_step_expr])
        , _ ) ->
          Ok
            ( Typed_ast.For
                (loc, typed_start_expr, typed_cond_expr, typed_step_expr, typed_loop_expr)
            , TEVoid )
      | _ ->
          (* Shouldn't occur! *)
          Error
            (Error.of_string
               (Fmt.str
                  "%s Something went wrong when type-checking for loop expression.@."
                  (string_of_loc loc))) )
  | Parsed_ast.BinOp (loc, bin_op, expr1, expr2) -> (
      infer_type_with_defns expr1 env
      >>= fun (typed_expr1, expr1_type) ->
      infer_type_with_defns expr2 env
      >>= fun (typed_expr2, expr2_type) ->
      if not (expr1_type = expr2_type) then
        Error
          (Error.of_string
             (Fmt.str
                "%s Type error - %s's  operands' types not consistent - they have type %s and %s@."
                (string_of_loc loc) (string_of_bin_op bin_op) (string_of_type expr1_type)
                (string_of_type expr2_type)))
      else
        let type_mismatch_error expected_type actual_type =
          Error
            (Error.of_string
               (Fmt.str
                  "%s Type error - %s expected operands of type %s, but they were of type %s@."
                  (string_of_loc loc) (string_of_bin_op bin_op)
                  (string_of_type expected_type)
                  (string_of_type actual_type))) in
        match bin_op with
        | BinOpPlus | BinOpMinus | BinOpMult | BinOpIntDiv | BinOpRem ->
            if expr1_type = TEInt then
              Ok (Typed_ast.BinOp (loc, TEInt, bin_op, typed_expr1, typed_expr2), TEInt)
            else type_mismatch_error TEInt expr1_type
        | BinOpLessThan | BinOpLessThanEq | BinOpGreaterThan | BinOpGreaterThanEq ->
            if expr1_type = TEInt then
              Ok (Typed_ast.BinOp (loc, TEBool, bin_op, typed_expr1, typed_expr2), TEBool)
            else type_mismatch_error TEInt expr1_type
        | BinOpAnd | BinOpOr ->
            if expr1_type = TEBool then
              Ok (Typed_ast.BinOp (loc, TEBool, bin_op, typed_expr1, typed_expr2), TEBool)
            else type_mismatch_error TEBool expr1_type
        | BinOpEq | BinOpNotEq ->
            Ok (Typed_ast.BinOp (loc, TEBool, bin_op, typed_expr1, typed_expr2), TEBool) )
  | Parsed_ast.UnOp (loc, un_op, expr) -> (
      let type_mismatch_error expected_type actual_type =
        Error
          (Error.of_string
             (Fmt.str
                "%s Type error - expected operand of type %s, but it was of type %s@."
                (string_of_loc loc)
                (string_of_type expected_type)
                (string_of_type actual_type))) in
      infer_type_with_defns expr env
      >>= fun (typed_expr, expr_type) ->
      match un_op with
      | UnOpNeg ->
          if expr_type = TEInt then
            Ok (Typed_ast.UnOp (loc, expr_type, un_op, typed_expr), TEInt)
          else type_mismatch_error TEInt expr_type
      | UnOpNot ->
          if expr_type = TEBool then
            Ok (Typed_ast.UnOp (loc, expr_type, un_op, typed_expr), TEBool)
          else type_mismatch_error TEBool expr_type )

(* Top level statement to infer type of overall program expr *)
let type_expr class_defns function_defns (expr : Parsed_ast.expr) =
  infer_type_expr class_defns function_defns (expr : Parsed_ast.expr) ([] : type_env)
  >>| fun (typed_expr, _expr_type) -> typed_expr
