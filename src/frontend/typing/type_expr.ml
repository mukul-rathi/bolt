open Ast.Ast_types
open Parsing
open Type_env
open Core

(* This checks the type of the expression is consistent with the field it's being assigned
   to in the constructor, and annotates it with the type if so *)
let type_constructor_arg class_defn type_expr_fn loc env
    (Parsed_ast.ConstructorArg (field_name, expr)) =
  let open Result in
  (* Check class has field and return its type if so *)
  get_class_field field_name class_defn loc
  >>= fun (TField (_, field_type, _, _)) ->
  (* check if expr being assigned type-checks and get its type*)
  type_expr_fn expr env
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

(* This checks the type of arguments passed to a function/method *)
let type_args type_expr_fn args env =
  let open Result in
  match args with
  | [] -> Ok ([], [TEVoid])
  | _  ->
      Result.all (List.map ~f:(fun expr -> type_expr_fn expr env) args)
      >>| fun typed_args_exprs_and_types -> List.unzip typed_args_exprs_and_types

(* Look up in env and class defns to get the types of an identifier *)
let type_identifier class_defns identifier env loc =
  let open Result in
  match identifier with
  | Parsed_ast.ObjField (var_name, field_name) ->
      (* Get the class definition to determine type of the field. *)
      get_obj_class_defn var_name env class_defns loc
      >>= fun (Parsed_ast.TClass (class_name, _, _, _) as class_defn) ->
      get_class_field field_name class_defn loc
      >>| fun (TField (_, field_type, _, _)) ->
      (* Convert to corresponding expr type to match the type declaration *)
      (Typed_ast.ObjField (class_name, var_name, field_type, field_name), field_type)
  | Parsed_ast.Variable var_name ->
      get_var_type var_name env loc
      >>| fun var_type -> (Typed_ast.Variable (var_type, var_name), var_type)

(* Given a parsed expr, we infer its type and return a tuple consisting of the
   type-annotated expr as the first value, and its type as the second value - the latter
   is explicitly returned since it is often used in recursive calls by the caller. *)
let rec type_expr class_defns function_defns (expr : Parsed_ast.expr) env =
  let open Result in
  let type_with_defns = type_expr class_defns function_defns in
  let type_block_with_defns = type_block_expr class_defns function_defns in
  (* Partially apply the function for brevity in recursive calls *)
  match expr with
  | Parsed_ast.Integer (loc, i) -> Ok (Typed_ast.Integer (loc, i), TEInt)
  | Parsed_ast.Boolean (loc, b) -> Ok (Typed_ast.Boolean (loc, b), TEBool)
  | Parsed_ast.Identifier (loc, id) ->
      type_identifier class_defns id env loc
      >>| fun (typed_id, id_type) -> (Typed_ast.Identifier (loc, typed_id), id_type)
  | Parsed_ast.Constructor (loc, class_name, constructor_args) ->
      (* Check that there is a matching class defn for the class name provided *)
      get_class_defn class_name class_defns loc
      >>= fun class_defn ->
      (* Check that all the constructor arguments type-check *)
      Result.all
        (List.map
           ~f:(type_constructor_arg class_defn type_with_defns loc env)
           constructor_args)
      >>| fun typed_constructor_args ->
      ( Typed_ast.Constructor (loc, TEClass class_name, class_name, typed_constructor_args)
      , TEClass class_name )
  | Parsed_ast.Let (loc, var_name, bound_expr) ->
      (* Infer type of expression that is being subbed and bind it to the let var*)
      check_variable_declarable var_name loc
      >>= fun () ->
      type_with_defns bound_expr env
      >>| fun (typed_bound_expr, bound_expr_type) ->
      (Typed_ast.Let (loc, bound_expr_type, var_name, typed_bound_expr), bound_expr_type)
  | Parsed_ast.Assign (loc, id, assigned_expr) ->
      check_identifier_assignable class_defns id env loc
      >>= fun () ->
      type_identifier class_defns id env loc
      >>= fun (typed_id, id_type) ->
      type_with_defns assigned_expr env
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
      check_identifier_consumable id loc
      >>= fun () ->
      type_identifier class_defns id env loc
      >>| fun (typed_id, id_type) -> (Typed_ast.Consume (loc, typed_id), id_type)
  | Parsed_ast.MethodApp (loc, var_name, method_name, args_exprs) ->
      get_var_type var_name env loc
      >>= fun obj_type ->
      get_obj_class_defn var_name env class_defns loc
      >>= fun class_defn ->
      get_method_type method_name class_defn loc
      >>= fun (param_types, return_type) ->
      type_args type_with_defns args_exprs env
      >>= fun (typed_args_exprs, args_types) ->
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
      type_args type_with_defns args_exprs env
      >>= fun (typed_args_exprs, args_types) ->
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
  | Parsed_ast.Printf (loc, format_str, args) ->
      (* Defer type checking of the overall printf expr to llvm codegen - as checked then
         for free *)
      type_args type_with_defns args env
      >>| fun (typed_args, _) -> (Typed_ast.Printf (loc, format_str, typed_args), TEVoid)
  | Parsed_ast.FinishAsync (loc, async_exprs, curr_thread_expr) ->
      (* Check async expressions type-check - note they have access to same env, as not
         being checked for data races in this stage of type-checking *)
      Result.all
        (List.map
           ~f:(fun (Parsed_ast.AsyncExpr async_block_expr) ->
             type_block_with_defns async_block_expr env
             >>| fun (typed_async_block_expr, _) ->
             Typed_ast.AsyncExpr typed_async_block_expr)
           async_exprs)
      >>= fun typed_async_exprs ->
      type_block_with_defns curr_thread_expr env
      >>| fun (typed_curr_thread_expr, curr_thread_expr_type) ->
      ( Typed_ast.FinishAsync
          (loc, curr_thread_expr_type, typed_async_exprs, typed_curr_thread_expr)
      , curr_thread_expr_type )
  (* We return type of the expr occurring on the current thread, not the forked thread *)
  | Parsed_ast.If (loc, cond_expr, then_expr, else_expr) -> (
      type_with_defns cond_expr env
      >>= fun (typed_cond_expr, cond_expr_type) ->
      type_block_with_defns then_expr env
      >>= fun (typed_then_expr, then_expr_type) ->
      type_block_with_defns else_expr env
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
      type_with_defns cond_expr env
      >>= fun (typed_cond_expr, cond_expr_type) ->
      type_block_with_defns loop_expr env
      >>= fun (typed_loop_expr, _) ->
      match cond_expr_type with
      | TEBool -> Ok (Typed_ast.While (loc, typed_cond_expr, typed_loop_expr), TEVoid)
      | _      ->
          Error
            (Error.of_string
               (Fmt.str
                  "%s Type error - Loop condition expression should have boolean type but instead has type %s@."
                  (string_of_loc loc)
                  (string_of_type cond_expr_type))) )
  | Parsed_ast.For
      (loc, start_expr, cond_expr, step_expr, Parsed_ast.Block (block_loc, loop_expr)) ->
      (* desugar into a while loop *)
      type_block_with_defns
        (Parsed_ast.Block
           ( loc
           , [ start_expr
             ; Parsed_ast.While
                 (loc, cond_expr, Parsed_ast.Block (block_loc, loop_expr @ [step_expr]))
             ] ))
        env
      >>| fun (typed_block_expr, block_expr_type) ->
      (Typed_ast.BlockExpr (loc, typed_block_expr), block_expr_type)
  | Parsed_ast.BinOp (loc, bin_op, expr1, expr2) -> (
      type_with_defns expr1 env
      >>= fun (typed_expr1, expr1_type) ->
      type_with_defns expr2 env
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
            if expr1_type = TEBool || expr1_type = TEInt then
              Ok (Typed_ast.BinOp (loc, TEBool, bin_op, typed_expr1, typed_expr2), TEBool)
            else
              Error
                (Error.of_string
                   (Fmt.str
                      "%s Type error - %s cannot have operands of type %s - only of type %s or %s @."
                      (string_of_loc loc) (string_of_bin_op bin_op)
                      (string_of_type expr1_type) (string_of_type TEInt)
                      (string_of_type TEBool))) )
  | Parsed_ast.UnOp (loc, un_op, expr) -> (
      let type_mismatch_error expected_type actual_type =
        Error
          (Error.of_string
             (Fmt.str
                "%s Type error - %s expected operand of type %s, but it was of type %s@."
                (string_of_loc loc) (string_of_un_op un_op)
                (string_of_type expected_type)
                (string_of_type actual_type))) in
      type_with_defns expr env
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

and type_block_expr class_defns function_defns (Parsed_ast.Block (loc, exprs)) env =
  let open Result in
  let type_with_defns = type_expr class_defns function_defns in
  let type_block_with_defns = type_block_expr class_defns function_defns in
  (* Partially apply the function for brevity in recursive calls *)
  check_no_var_shadowing_in_block exprs loc
  >>= fun () ->
  match exprs with
  | []                      ->
      Error
        (Error.of_string
           (Fmt.str "%s Type error - block of expressions is empty@." (string_of_loc loc)))
  | [expr]                  ->
      type_with_defns expr env
      >>| fun (typed_expr, expr_type) ->
      (Typed_ast.Block (loc, expr_type, [typed_expr]), expr_type)
  | expr1 :: expr2 :: exprs ->
      type_with_defns expr1 env
      >>= fun (typed_expr1, expr1_type) ->
      (* Need to update env for subsequent expressions in block with let-binding if
         previous expr was a let-binding *)
      (let updated_env =
         match typed_expr1 with
         | Typed_ast.Let (_, _, var_name, _) -> (var_name, expr1_type) :: env
         | _ -> env in
       type_block_with_defns (Parsed_ast.Block (loc, expr2 :: exprs)) updated_env)
      >>| fun (Typed_ast.Block (_, _, typed_exprs), block_expr_type) ->
      (Typed_ast.Block (loc, block_expr_type, typed_expr1 :: typed_exprs), block_expr_type)
