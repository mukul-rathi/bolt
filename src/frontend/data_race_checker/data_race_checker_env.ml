open Ast.Ast_types
open Core
open Desugaring.Desugared_ast

let get_class_defn class_name class_defns =
  let matching_class_defns =
    List.filter ~f:(fun (TClass (name, _, _, _)) -> class_name = name) class_defns in
  List.hd_exn matching_class_defns

(* This should never throw an exception since we've checked this property in earlier
   type-checking stages of the pipeline *)

let get_class_regions class_name class_defns =
  get_class_defn class_name class_defns |> fun (TClass (_, regions, _, _)) -> regions

let get_class_field field_name (TClass (_, _, field_defns, _)) =
  let matching_field_defns =
    List.filter ~f:(fun (TField (_, _, name, _)) -> field_name = name) field_defns in
  List.hd_exn matching_field_defns

(* This should never throw an exception since we've checked this property in earlier
   type-checking stages of the pipeline *)

let rec elem_in_list x = function [] -> false | y :: ys -> x = y || elem_in_list x ys

let class_has_capability class_name cap class_defns =
  let rec class_has_capability_helper class_name cap class_defns seen_class_names =
    if elem_in_list class_name seen_class_names then
      (* Avoid infinite recursion on type definition *)
      false
    else
      get_class_defn class_name class_defns
      |> fun (TClass (_, regions, fields, _)) ->
      match cap with
      (* any one of its regions (and nested field types) hold the capability *)
      | Linear | Subordinate | Thread ->
          List.exists ~f:(fun (TRegion (region_cap, _)) -> region_cap = cap) regions
          || List.exists
               ~f:(fun (TField (_, field_type, _, _)) ->
                 match field_type with
                 | TEClass (nested_class, _) ->
                     class_has_capability_helper nested_class cap class_defns
                       (class_name :: seen_class_names)
                 | _                         -> false)
               fields
      | Read                          ->
          List.for_all ~f:(fun (TRegion (region_cap, _)) -> region_cap = Read) regions
      | Safe                          ->
          List.for_all
            ~f:(fun (TRegion (region_cap, _)) -> region_cap = Read || region_cap = Locked)
            regions
      | Locked                        ->
          class_has_capability_helper class_name Safe class_defns seen_class_names
          && List.exists ~f:(fun (TRegion (region_cap, _)) -> region_cap = Locked) regions
  in
  class_has_capability_helper class_name cap class_defns []

let get_class_field_regions class_name field_name class_defns =
  get_class_defn class_name class_defns
  |> fun (TClass (_, regions, _, _) as class_defn) ->
  get_class_field field_name class_defn
  |> fun (TField (_, _, _, field_region_names)) ->
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
