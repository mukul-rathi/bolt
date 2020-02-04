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

and update_var_capabilities_block_expr var_name caps_update_fn (Block (_, _, exprs)) =
  List.iter ~f:(update_var_capabilities_expr var_name caps_update_fn) exprs
