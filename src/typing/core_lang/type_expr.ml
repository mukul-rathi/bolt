open Ast.Ast_types
open Parsing
open Core_lang_type_env
open Core
open Result

let check_no_var_shadowing_in_block exprs loc =
  if
    List.contains_dup
      ~compare:(fun expr1 expr2 ->
        match expr1 with
        | Parsing.Parsed_ast.Let (_, var_name1, _) -> (
          match expr2 with
          | Parsing.Parsed_ast.Let (_, var_name2, _) ->
              if var_name1 = var_name2 then 0 (* duplicate let binding! *) else 1
          | _ -> 1 )
        | _ -> 1)
      exprs
  then
    Error
      (Error.of_string
         (Fmt.str "%s Type error: Duplicate variable declarations in same block.@."
            (string_of_loc loc)))
  else Ok ()

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
  | Parsed_ast.Unit loc -> Ok (Typed_ast.Unit loc, TEUnit)
  | Parsed_ast.Integer (loc, i) -> Ok (Typed_ast.Integer (loc, i), TEInt)
  | Parsed_ast.Boolean (loc, b) -> Ok (Typed_ast.Boolean (loc, b), TEBool)
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
  | Parsed_ast.Let (loc, var_name, bound_expr) ->
      (* Infer type of expression that is being subbed and bind it to the let var*)
      infer_type_with_defns bound_expr env
      >>| fun (typed_bound_expr, bound_expr_type) ->
      (Typed_ast.Let (loc, bound_expr_type, var_name, typed_bound_expr), bound_expr_type)
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
  | Parsed_ast.ObjMethod (loc, var_name, method_name, args_exprs) ->
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
          ( Typed_ast.ObjMethod
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
  | Parsed_ast.FinishAsync (loc, async_expr1, async_expr2) ->
      (* Check async expressions type-check - note they have access to same env, as not
         being checked for data races in this stage of type-checking *)
      infer_type_with_defns async_expr1 env
      >>= fun (typed_async_expr1, async_expr1_type) ->
      infer_type_with_defns async_expr2 env
      >>| fun (typed_async_expr2, _) ->
      ( Typed_ast.FinishAsync (loc, async_expr1_type, typed_async_expr1, typed_async_expr2)
      , async_expr1_type )
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
      | TEBool -> Ok (Typed_ast.While (loc, typed_cond_expr, typed_loop_expr), TEUnit)
      | _      ->
          Error
            (Error.of_string
               (Fmt.str
                  "%s Type error - While loop condition expression should have boolean type but instead has type %s@."
                  (string_of_loc loc)
                  (string_of_type cond_expr_type))) )
  | Parsed_ast.For (loc, loop_var, start_expr, end_expr, step_expr, loop_expr) ->
      (* Loop variable is an int - so check its start val, end val and the amount it's
         being stepped by each loop are all ints too *)
      let for_loop_error part actual_type =
        Error
          (Error.of_string
             (Fmt.str
                "%s Type error - For loop range expression - %s should have type %s but instead has type %s@."
                (string_of_loc loc) part (string_of_type TEInt)
                (string_of_type actual_type))) in
      infer_type_with_defns start_expr env
      >>= fun (typed_start_expr, start_expr_type) ->
      ( match start_expr_type with
      | TEInt -> Ok ()
      | _     -> for_loop_error "start" start_expr_type )
      >>= fun () ->
      infer_type_with_defns end_expr env
      >>= fun (typed_end_expr, end_expr_type) ->
      (match end_expr_type with TEInt -> Ok () | _ -> for_loop_error "end" end_expr_type)
      >>= fun () ->
      infer_type_with_defns step_expr env
      >>= fun (typed_step_expr, step_expr_type) ->
      ( match step_expr_type with
      | TEInt -> Ok ()
      | _     -> for_loop_error "step" step_expr_type )
      >>= fun () ->
      infer_type_with_defns loop_expr ((loop_var, TEInt) :: env)
      >>| fun (typed_loop_expr, _) ->
      ( Typed_ast.For
          ( loc
          , loop_var
          , typed_start_expr
          , typed_end_expr
          , typed_step_expr
          , typed_loop_expr )
      , TEUnit )
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
  | Parsed_ast.UnOp (loc, UnOpNot, expr) -> (
      infer_type_with_defns expr env
      >>= fun (typed_expr, expr_type) ->
      match expr_type with
      | TEBool -> Ok (Typed_ast.UnOp (loc, TEBool, UnOpNot, typed_expr), TEBool)
      | _      ->
          Error
            (Error.of_string
               (Fmt.str
                  "%s Type error - %s expected operand of type %s, but it was of type %s@."
                  (string_of_loc loc) (string_of_un_op UnOpNot) (string_of_type TEBool)
                  (string_of_type expr_type))) )

(* Top level statement to infer type of overall program expr *)
let type_expr class_defns trait_defns function_defns (expr : Parsed_ast.expr) =
  infer_type_expr class_defns trait_defns function_defns
    (expr : Parsed_ast.expr)
    ([] : type_env)
  >>| fun (typed_expr, _expr_type) -> typed_expr