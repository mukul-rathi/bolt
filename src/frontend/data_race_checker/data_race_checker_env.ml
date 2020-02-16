open Ast.Ast_types
open Core
open Desugaring.Desugared_ast

let rec elem_in_list x = function [] -> false | y :: ys -> x = y || elem_in_list x ys
let intersect_lists list1 list2 = List.filter ~f:(fun x -> elem_in_list x list2) list1

let get_class_defn class_name class_defns =
  let matching_class_defns =
    List.filter ~f:(fun (TClass (name, _, _, _)) -> class_name = name) class_defns in
  (* This should never throw an exception since we've checked this property in earlier
     type-checking stages of the pipeline *)
  List.hd_exn matching_class_defns

let get_class_regions class_name class_defns =
  get_class_defn class_name class_defns |> fun (TClass (_, regions, _, _)) -> regions

let get_class_field field_name (TClass (_, _, field_defns, _)) =
  let matching_field_defns =
    List.filter ~f:(fun (TField (_, _, name, _)) -> field_name = name) field_defns in
  (* This should never throw an exception since we've checked this property in earlier
     type-checking stages of the pipeline *)
  List.hd_exn matching_field_defns

let get_class_field_regions class_name field_name class_defns =
  get_class_defn class_name class_defns
  |> fun (TClass (_, regions, _, _) as class_defn) ->
  get_class_field field_name class_defn
  |> fun (TField (_, _, _, field_region_names)) ->
  List.filter
    ~f:(fun (TRegion (_, region_name)) -> elem_in_list region_name field_region_names)
    regions

let get_class_region_fields class_name region_name class_defns =
  get_class_defn class_name class_defns
  |> fun (TClass (_, _, fields, _)) ->
  List.filter
    ~f:(fun (TField (_, _, _, field_region_names)) ->
      elem_in_list region_name field_region_names)
    fields

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
      (* all its regions hold the capability *)
      | Read | Encapsulated           ->
          List.for_all ~f:(fun (TRegion (region_cap, _)) -> region_cap = cap) regions
      | Safe                          ->
          List.for_all
            ~f:(fun (TRegion (region_cap, _)) -> region_cap = Read || region_cap = Locked)
            regions
      | Locked                        ->
          class_has_capability_helper class_name Safe class_defns seen_class_names
          && List.exists ~f:(fun (TRegion (region_cap, _)) -> region_cap = Locked) regions
  in
  class_has_capability_helper class_name cap class_defns []

let type_has_capability type_expr cap class_defns =
  match type_expr with
  | TEClass (class_name, _) -> class_has_capability class_name cap class_defns
  | _                       -> false

let region_fields_have_capability (TRegion (region_cap, region_name)) class_name cap
    class_defns =
  region_cap = cap
  || get_class_region_fields class_name region_name class_defns
     |> fun fields_in_region ->
     List.exists
       ~f:(fun (TField (_, field_type, _, _)) ->
         match field_type with
         | TEClass (field_class, _) -> class_has_capability field_class cap class_defns
         | _                        -> false)
       fields_in_region

(* There is another region in the class that can both access subordinate state in one
   region and also subordinate state in another region - thus acting as a channel for
   them. *)
let regions_have_subord_channel class_name class_defns region_1_name region_2_name =
  let get_reg_subord_fields region_name =
    List.filter
      ~f:(fun (TField (_, field_type, _, _)) ->
        type_has_capability field_type Subordinate class_defns)
      (get_class_region_fields class_name region_name class_defns) in
  (* collect the regions that aren't region 1 or region 2 and have access to subord state*)
  let get_potential_channel_regions sub_ord_fields =
    List.concat_map
      ~f:(fun (TField (_, _, _, field_reg_names)) ->
        List.filter
          ~f:(fun reg_name ->
            (not (reg_name = region_1_name)) && not (reg_name = region_2_name))
          field_reg_names)
      sub_ord_fields in
  let get_region_1_potential_channels =
    get_potential_channel_regions (get_reg_subord_fields region_1_name) in
  let get_region_2_potential_channels =
    get_potential_channel_regions (get_reg_subord_fields region_1_name) in
  (* check if a region in intersection of these potential channels *)
  let subord_channels =
    intersect_lists get_region_1_potential_channels get_region_2_potential_channels in
  List.length subord_channels > 0

(* Check that overlapping fields have safe capability and are not subordinate *)
let regions_have_safe_unsubord_shared_state class_name class_defns region_1_name
    region_2_name =
  let region_1_fields = get_class_region_fields class_name region_1_name class_defns in
  let region_2_fields = get_class_region_fields class_name region_2_name class_defns in
  let shared_fields = intersect_lists region_1_fields region_2_fields in
  List.for_all
    ~f:(fun (TField (_, field_type, _, _)) ->
      (not (type_has_capability field_type Subordinate class_defns))
      && type_has_capability field_type Safe class_defns)
    shared_fields

let can_concurrently_access_regions class_name class_defns
    (TRegion (region_1_cap, region_1_name)) (TRegion (_, region_2_name)) =
  regions_have_safe_unsubord_shared_state class_name class_defns region_1_name
    region_2_name
  && (not
        (regions_have_subord_channel class_name class_defns region_1_name region_2_name))
  (* Can't access the same linear region in multiple threads as violates linearity *)
  && not (region_1_cap = Linear && region_1_name = region_2_name)

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
