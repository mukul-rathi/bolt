open Ast.Ast_types
open Core
open Data_race_checker.Data_race_checker_ast

(* Name mangles method name so doesn't clash with other methods/functions *)
let ir_gen_method_name meth_name class_name =
  Fmt.str "_%s_%s" (Class_name.to_string class_name) (Method_name.to_string meth_name)

let get_class_defn class_name class_defns =
  let matching_class_defns =
    List.filter ~f:(fun (TClass (name, _, _, _)) -> class_name = name) class_defns in
  match matching_class_defns with
  | []           ->
      Error
        (Error.of_string
           (Fmt.str "Type error - Class %s not defined in environment@."
              (Class_name.to_string class_name)))
  | [class_defn] -> Ok class_defn
  | _            ->
      Error
        (Error.of_string
           (Fmt.str "Type error - Class %s has duplicate definitions in environment@."
              (Class_name.to_string class_name)))

let rec get_class_field_index class_name field_defns field_name index =
  match field_defns with
  | []                            ->
      Error
        (Error.of_string
           (Fmt.str "Type error - Class %s doesn't contain field %s@."
              (Class_name.to_string class_name)
              (Field_name.to_string field_name)))
  | TField (_, _, name, _) :: fds ->
      if name = field_name then Ok index
      else get_class_field_index class_name fds field_name (index + 1)

(* Given a field and the type of the object to which it belongs, and a list of class
   defns, get the field index within the list of field defns in the corresponding class
   defn *)
let ir_gen_field_index field_name class_name class_defns =
  let open Result in
  get_class_defn class_name class_defns
  >>= fun (TClass (class_name, _, field_defns, _)) ->
  (* first two fields in a class are reserved for threadID and lockCount *)
  get_class_field_index class_name field_defns field_name 2
