open Ast.Ast_types
open Core

let get_class_defn class_name class_defns =
  let matching_class_defns =
    List.filter
      ~f:(fun (Typing.Typed_ast.TClass (name, _, _, _)) -> class_name = name)
      class_defns in
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
  get_class_defn class_name class_defns
  >>| fun (Typing.Typed_ast.TClass (_, regions, _, _)) -> regions

let get_class_field field_name (Typing.Typed_ast.TClass (_, _, field_defns, _)) =
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
  >>= fun (Typing.Typed_ast.TClass (_, regions, _, _) as class_defn) ->
  get_class_field field_name class_defn
  >>| fun (TField (_, _, _, field_region_names)) ->
  List.filter
    ~f:(fun (TRegion (_, region_name)) -> elem_in_list region_name field_region_names)
    regions

(* Used when tracking aliasing *)
let rec reduce_expr_to_id expr =
  let open Data_race_checker_ast in
  match expr with
  | Integer _ | Boolean _ -> None
  | Identifier (_, id) -> Some [id]
  | BlockExpr (_, block_expr) -> reduce_block_expr_to_id block_expr
  | Constructor (_, _, _, _) -> None
  | Let (_, _, _, bound_expr) -> reduce_expr_to_id bound_expr
  | Assign (_, _, _, assigned_expr) -> reduce_expr_to_id assigned_expr
  | Consume (_, id) -> Some [id]
  | MethodApp (_, _, _, _, _, _) -> None
  | FunctionApp (_, _, _, _) -> None
  | Printf (_, _, _) -> None
  | FinishAsync (_, _, _, _, curr_thread_expr) -> reduce_block_expr_to_id curr_thread_expr
  | If (_, _, _, then_expr, else_expr) -> (
      let then_maybe_id = reduce_block_expr_to_id then_expr in
      let else_maybe_id = reduce_block_expr_to_id else_expr in
      match then_maybe_id with
      | None         -> else_maybe_id
      | Some then_id -> (
        match else_maybe_id with
        | None         -> Some then_id
        | Some else_id -> Some (then_id @ else_id) ) )
  | While _ -> None
  | BinOp _ -> None
  | UnOp _ -> None

and reduce_block_expr_to_id (Block (loc, type_expr, exprs)) =
  match exprs with
  | []             -> None
  | [expr]         -> reduce_expr_to_id expr
  | _ :: rem_exprs -> reduce_block_expr_to_id (Block (loc, type_expr, rem_exprs))

(* Check if the expression is reduced to an id that matches a given var_name *)
let var_in_expr_reduced_ids var_name optional_ids =
  let open Data_race_checker_ast in
  match optional_ids with
  | Some ids ->
      List.length
        (List.filter
           ~f:(function
             | Variable (_, name, _, _) -> var_name = name
             | ObjField (_, obj_name, _, _, _, _) -> var_name = obj_name)
           ids)
      > 0
  | None     -> false

let update_var_capabilities_identifier var_name caps_update_fn id =
  let open Data_race_checker_ast in
  match id with
  | Variable (_, name, _, caps) -> if var_name = name then caps_update_fn caps
  | ObjField (_, obj_name, _, _, _, caps) ->
      if var_name = obj_name then caps_update_fn caps

let rec update_var_capabilities_expr var_name caps_update_fn expr =
  let open Data_race_checker_ast in
  let update_var_caps_expr_rec = update_var_capabilities_expr var_name caps_update_fn in
  let update_var_caps_block_expr_rec =
    update_var_capabilities_block_expr var_name caps_update_fn in
  let update_var_caps_identifier_rec =
    update_var_capabilities_identifier var_name caps_update_fn in
  match expr with
  | Integer _ | Boolean _ -> ()
  | Identifier (_, id) -> update_var_caps_identifier_rec id
  | BlockExpr (_, block_expr) -> update_var_caps_block_expr_rec block_expr
  | Constructor (_, _, _, constructor_args) ->
      List.iter
        ~f:(fun (ConstructorArg (_, _, expr)) -> update_var_caps_expr_rec expr)
        constructor_args
  | Let (_, _, _, bound_expr) -> update_var_caps_expr_rec bound_expr
  | Assign (_, _, id, assigned_expr) ->
      update_var_caps_identifier_rec id ;
      update_var_caps_expr_rec assigned_expr
  | Consume (_, id) -> update_var_caps_identifier_rec id
  | MethodApp (_, _, _, _, _, args) -> List.iter ~f:update_var_caps_expr_rec args
  | FunctionApp (_, _, _, args) -> List.iter ~f:update_var_caps_expr_rec args
  | Printf (_, _, args) -> List.iter ~f:update_var_caps_expr_rec args
  | FinishAsync (_, _, async_exprs, _, curr_thread_expr) ->
      List.iter
        ~f:(fun (AsyncExpr (_, expr)) -> update_var_caps_block_expr_rec expr)
        async_exprs ;
      update_var_caps_block_expr_rec curr_thread_expr
  | If (_, _, cond_expr, then_expr, else_expr) ->
      update_var_caps_expr_rec cond_expr ;
      update_var_caps_block_expr_rec then_expr ;
      update_var_caps_block_expr_rec else_expr
  | While (_, cond_expr, loop_expr) ->
      update_var_caps_expr_rec cond_expr ;
      update_var_caps_block_expr_rec loop_expr
  | BinOp (_, _, _, expr1, expr2) ->
      update_var_caps_expr_rec expr1 ;
      update_var_caps_expr_rec expr2
  | UnOp (_, _, _, expr) -> update_var_caps_expr_rec expr

and update_var_capabilities_block_expr var_name caps_update_fn
    (Block (loc, type_block_expr, exprs)) =
  match exprs with
  | []                      -> ()
  | expr :: remaining_exprs ->
      ( match expr with
      (* check if we have alias to an object, if so then it is also affected by the update *)
      | Let (_, _, name, bound_expr) ->
          update_var_capabilities_expr var_name caps_update_fn bound_expr ;
          if var_in_expr_reduced_ids var_name (reduce_expr_to_id bound_expr) then
            update_var_capabilities_block_expr name caps_update_fn
              (Block (loc, type_block_expr, remaining_exprs))
      | _ -> () ) ;
      update_var_capabilities_block_expr var_name caps_update_fn
        (Block (loc, type_block_expr, remaining_exprs))
