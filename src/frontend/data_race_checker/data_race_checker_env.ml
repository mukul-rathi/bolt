open Ast.Ast_types
open Core
open Desugaring.Desugared_ast

let get_class_defn class_name class_defns =
  let matching_class_defns =
    List.filter ~f:(fun (TClass (name, _, _, _)) -> class_name = name) class_defns in
  match matching_class_defns with
  | [class_defn] -> Ok class_defn
  | _            ->
      Error
        (Error.of_string
           (Fmt.str
              "Something went wrong - couldn't get unique class definition for %s. @."
              (Class_name.to_string class_name)))

let get_class_regions class_name class_defns =
  let open Result in
  get_class_defn class_name class_defns >>| fun (TClass (_, regions, _, _)) -> regions

let get_class_field field_name (TClass (_, _, field_defns, _)) =
  let matching_class_defns =
    List.filter ~f:(fun (TField (_, _, name, _)) -> field_name = name) field_defns in
  match matching_class_defns with
  | [field] -> Ok field
  | _       ->
      Error
        (Error.of_string
           (Fmt.str
              "Something went wrong - couldn't get unique field definition for %s. @."
              (Field_name.to_string field_name)))

let rec elem_in_list x = function [] -> false | y :: ys -> x = y || elem_in_list x ys

let get_class_field_regions class_name field_name class_defns =
  let open Result in
  get_class_defn class_name class_defns
  >>= fun (TClass (_, regions, _, _) as class_defn) ->
  get_class_field field_name class_defn
  >>| fun (TField (_, _, _, field_region_names)) ->
  List.filter
    ~f:(fun (TRegion (_, region_name)) -> elem_in_list region_name field_region_names)
    regions

let rec reduce_expr_to_obj_id expr =
  match expr with
  | Integer _ | Boolean _ -> []
  | Identifier (_, id) -> [id]
  | BlockExpr (_, block_expr) -> reduce_block_expr_to_obj_id block_expr
  | Constructor (_, _, _, _) -> []
  | Let (_, _, _, bound_expr) -> reduce_expr_to_obj_id bound_expr
  | Assign (_, _, _, assigned_expr) -> reduce_expr_to_obj_id assigned_expr
  | Consume (_, _) -> []
  | MethodApp (_, _, _, _, _, _) -> []
  | FunctionApp (_, _, _, _) -> []
  | Printf (_, _, _) -> []
  | FinishAsync (_, _, _, _, curr_thread_expr) ->
      reduce_block_expr_to_obj_id curr_thread_expr
  | If (_, _, _, then_expr, else_expr) ->
      let then_id = reduce_block_expr_to_obj_id then_expr in
      let else_id = reduce_block_expr_to_obj_id else_expr in
      then_id @ else_id
  | While _ -> []
  | BinOp _ -> [] (* Bin op returns either a TEInt or a Bool *)
  | UnOp _ -> []

and reduce_block_expr_to_obj_id (Block (loc, type_expr, exprs)) =
  match exprs with
  | []             -> []
  | [expr]         -> reduce_expr_to_obj_id expr
  | _ :: rem_exprs -> reduce_block_expr_to_obj_id (Block (loc, type_expr, rem_exprs))
