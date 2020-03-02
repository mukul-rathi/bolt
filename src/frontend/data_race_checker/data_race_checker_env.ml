open Ast.Ast_types
open Core
open Desugaring.Desugared_ast

let rec elem_in_list x = function [] -> false | y :: ys -> x = y || elem_in_list x ys
let intersect_lists list1 list2 = List.filter ~f:(fun x -> elem_in_list x list2) list1
let is_subset_of xs ys = List.for_all ~f:(fun x -> elem_in_list x ys) xs

let var_lists_are_equal xs ys =
  let compare_fn x y = String.compare (Var_name.to_string x) (Var_name.to_string y) in
  let deduped_xs = List.dedup_and_sort ~compare:compare_fn xs in
  let deduped_ys = List.dedup_and_sort ~compare:compare_fn ys in
  List.equal (fun x y -> x = y) deduped_xs deduped_ys

let identifier_matches_var_name var_name = function
  | Variable (_, name, _)       -> name = var_name
  | ObjField (_, name, _, _, _) -> name = var_name

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

(* Convert a parameter to a representation which contains the regions it is allowed to
   access. *)
let param_to_obj_var_and_regions class_defns
    (TParam (type_expr, param_name, maybe_region_guards)) =
  match type_expr with
  | TEClass (param_class, _) ->
      let class_regions = get_class_regions param_class class_defns in
      let obj_regions =
        match maybe_region_guards with
        | None               -> class_regions (* no constraints so can access anything *)
        | Some region_guards ->
            List.filter
              ~f:(fun (TRegion (_, reg_name)) -> elem_in_list reg_name region_guards)
              class_regions in
      Some (param_name, param_class, obj_regions)
  | _                        ->
      (* not an object so ignore *)
      None

let get_function_params func_name function_defns =
  List.hd_exn
    (List.filter_map
       ~f:(fun (TFunction (name, _, params, _)) ->
         if name = func_name then Some params else None)
       function_defns)

let get_method_params class_name meth_name class_defns =
  get_class_defn class_name class_defns
  |> fun (TClass (_, _, _, method_defns)) ->
  List.hd_exn
    (List.filter_map
       ~f:(fun (TMethod (name, _, params, _, _)) ->
         if name = meth_name then Some params else None)
       method_defns)

let params_to_obj_vars_and_regions class_defns params =
  List.filter_map ~f:(param_to_obj_var_and_regions class_defns) params

let get_identifier_name = function
  | Variable (_, name, _)       -> name
  | ObjField (_, name, _, _, _) -> name

let get_identifier_regions = function
  | Variable (_, _, regions) -> regions
  | ObjField (_, _, _, _, regions) -> regions

let get_method_effect_regions class_name meth_name class_defns =
  get_class_defn class_name class_defns
  |> fun (TClass (_, _, _, method_defns)) ->
  List.hd_exn
    (List.filter_map
       ~f:(fun (TMethod (name, _, _, effect_regions, _)) ->
         if name = meth_name then Some effect_regions else None)
       method_defns)

let set_identifier_regions id new_regions =
  match id with
  | Variable (var_type, var_name, _) -> Variable (var_type, var_name, new_regions)
  | ObjField (obj_class, obj_name, field_type, field_name, _) ->
      ObjField (obj_class, obj_name, field_type, field_name, new_regions)

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
      | Linear | Subordinate | ThreadLocal ->
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
      | Read | Encapsulated ->
          List.for_all ~f:(fun (TRegion (region_cap, _)) -> region_cap = cap) regions
      | ThreadSafe ->
          List.for_all
            ~f:(fun (TRegion (region_cap, _)) -> region_cap = Read || region_cap = Locked)
            regions
      | Locked ->
          class_has_capability_helper class_name ThreadSafe class_defns seen_class_names
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

let identifier_has_capability id cap class_defns =
  let check_region_capabilities class_name regions =
    List.exists
      ~f:(fun region -> region_fields_have_capability region class_name cap class_defns)
      regions in
  match id with
  | Variable (var_type, _, regions) -> (
    match var_type with
    | TEClass (var_class, _) -> check_region_capabilities var_class regions
    | _                      -> false )
  | ObjField (obj_class, _, _, _, regions) -> check_region_capabilities obj_class regions

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

(* Check that overlapping fields are not subordinate *)
let regions_have_no_subord_shared_state class_name class_defns region_1_name region_2_name
    =
  let region_1_fields = get_class_region_fields class_name region_1_name class_defns in
  let region_2_fields = get_class_region_fields class_name region_2_name class_defns in
  let shared_fields = intersect_lists region_1_fields region_2_fields in
  List.for_all
    ~f:(fun (TField (_, field_type, _, _)) ->
      not (type_has_capability field_type Subordinate class_defns))
    shared_fields

(* Check that overlapping fields are safe - i.e. either we're accessing them with a safe
   capability, or they themselves are safe *)
let regions_have_safe_shared_state class_name class_defns
    (TRegion (region_1_cap, region_1_name)) (TRegion (region_2_cap, region_2_name)) =
  let regions_capabilities_are_safe region_1_cap region2_cap =
    (region_1_cap = Locked || region_1_cap = Read)
    && (region2_cap = Locked || region2_cap = Read) in
  let region_1_fields = get_class_region_fields class_name region_1_name class_defns in
  let region_2_fields = get_class_region_fields class_name region_2_name class_defns in
  let shared_fields = intersect_lists region_1_fields region_2_fields in
  regions_capabilities_are_safe region_1_cap region_2_cap
  || List.for_all
       ~f:(fun (TField (_, field_type, _, _)) ->
         type_has_capability field_type ThreadSafe class_defns)
       shared_fields

let can_concurrently_access_regions class_name class_defns
    (TRegion (region_1_cap, region_1_name) as region1)
    (TRegion (_, region_2_name) as region2) =
  regions_have_safe_shared_state class_name class_defns region1 region2
  && regions_have_no_subord_shared_state class_name class_defns region_1_name
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
