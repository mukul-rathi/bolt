open Core
open Result
open Ast.Ast_types

let check_no_duplicate_trait_names trait_defns =
  if
    List.contains_dup
      ~compare:(fun (TTrait (name_1, _, _)) (TTrait (name_2, _, _)) ->
        if name_1 = name_2 then 0 else 1)
      trait_defns
  then
    Error
      (Error.of_string
         (Fmt.str "Duplicate trait declarations. Traits must have distinct names@."))
  else Ok ()

(* Helper function to check if a field is const *)
let check_field_const = function
  | TRequire (TField (MConst, _name, _type)) -> true
  | _ -> false

(* If a trait has a read capability, make sure it only has const fields *)
let type_trait_defn = function
  | TTrait (name, Read, require_field_defns) ->
      if List.for_all ~f:check_field_const require_field_defns then Ok ()
      else
        Error
          (Error.of_string
             (Fmt.str "%s is a read trait but its fields aren't const.@."
                (Trait_name.to_string name)))
  | _ -> Ok ()

let type_trait_defns trait_defns =
  check_no_duplicate_trait_names trait_defns
  >>= fun () -> Result.all_unit (List.map ~f:type_trait_defn trait_defns)
