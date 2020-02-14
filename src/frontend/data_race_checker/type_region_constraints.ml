open Core
open Data_race_checker_ast
open Ast.Ast_types

let type_regions_constraints_identifier id loc =
  let error_msg =
    Error
      (Error.of_string
         (Fmt.str "%s Potential data race: no allowed regions for %s@."
            (string_of_loc loc) (string_of_id id))) in
  match id with
  | Variable (var_type, _, regions) -> (
    match var_type with
    | TEClass _ -> if regions = [] then error_msg else Ok ()
    | _         -> Ok () )
  | ObjField (_, _, _, _, regions) -> if regions = [] then error_msg else Ok ()

let rec type_regions_constraints_expr expr =
  let open Result in
  match expr with
  | Integer _ | Boolean _ -> Ok ()
  | Identifier (loc, id) -> type_regions_constraints_identifier id loc
  | BlockExpr (_, block_expr) -> type_regions_constraints_block_expr block_expr
  | Constructor (_, _, _, constructor_args) ->
      Result.all_unit
        (List.map
           ~f:(fun (ConstructorArg (_, _, expr)) -> type_regions_constraints_expr expr)
           constructor_args)
  | Let (_, _, _, bound_expr) -> type_regions_constraints_expr bound_expr
  | Assign (loc, _, id, assigned_expr) ->
      type_regions_constraints_identifier id loc
      >>= fun () -> type_regions_constraints_expr assigned_expr
  | Consume (loc, id) -> type_regions_constraints_identifier id loc
  | MethodApp (_, _, _, _, _, args) ->
      Result.all_unit (List.map ~f:type_regions_constraints_expr args)
  | FunctionApp (_, _, _, args) ->
      Result.all_unit (List.map ~f:type_regions_constraints_expr args)
  | Printf (_, _, args) ->
      Result.all_unit (List.map ~f:type_regions_constraints_expr args)
  | FinishAsync (_, _, async_exprs, _, curr_thread_expr) ->
      Result.all_unit
        (List.map
           ~f:(fun (AsyncExpr (_, expr)) -> type_regions_constraints_block_expr expr)
           async_exprs)
      >>= fun () -> type_regions_constraints_block_expr curr_thread_expr
  | If (_, _, cond_expr, then_expr, else_expr) ->
      type_regions_constraints_expr cond_expr
      >>= fun () ->
      type_regions_constraints_block_expr then_expr
      >>= fun () -> type_regions_constraints_block_expr else_expr
  | While (_, cond_expr, loop_expr) ->
      type_regions_constraints_expr cond_expr
      >>= fun () -> type_regions_constraints_block_expr loop_expr
  | BinOp (_, _, _, expr1, expr2) ->
      type_regions_constraints_expr expr1
      >>= fun () -> type_regions_constraints_expr expr2
  | UnOp (_, _, _, expr) -> type_regions_constraints_expr expr

and type_regions_constraints_block_expr (Block (_, _, exprs)) =
  Result.all_unit (List.map ~f:type_regions_constraints_expr exprs)
