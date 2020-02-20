open Core
open Desugaring.Desugared_ast
open Data_race_checker_env
open Ast.Ast_types

let type_linear_obj_method_args class_defns obj_name obj_class args_ids loc =
  if class_has_capability obj_class Linear class_defns then
    if List.exists ~f:(identifier_matches_var_name obj_name) args_ids then
      Error
        (Error.of_string
           (Fmt.str "%s One of linear object %s's method's arguments aliases it@."
              (string_of_loc loc) (Var_name.to_string obj_name)))
    else Ok () (* no aliasing in arguments *)
  else (* not linear so we don't care *) Ok ()

let type_linear_args class_defns args_ids loc =
  let linear_args_ids =
    List.filter
      ~f:(fun arg_id -> identifier_has_capability arg_id Linear class_defns)
      args_ids in
  let matching_ids = function
    | Variable (_, var_name, _) ->
        List.filter ~f:(identifier_matches_var_name var_name) args_ids
    | ObjField _ as id          -> List.filter ~f:(fun arg_id -> id = arg_id) args_ids
  in
  (* for all linear identifiers, make sure no other identifier matches that linear
     identifier *)
  if
    List.for_all
      ~f:(fun linear_arg_id -> List.length (matching_ids linear_arg_id) = 1)
      linear_args_ids
  then Ok ()
  else
    Error
      (Error.of_string
         (Fmt.str "%s Linear arguments are duplicated@." (string_of_loc loc)))
