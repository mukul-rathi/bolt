open Core
open Desugaring.Desugared_ast
open Ast.Ast_types
open Type_concurrent_capability_access
open Data_race_checker_env

let type_subtyping_capabilities class_defns class_name superclass =
  get_class_capabilities superclass class_defns
  |> fun superclass_capabilities ->
  Result.all_unit
    (List.map2_exn
       ~f:(fun superclass_cap1 superclass_cap2 ->
         if
           (* if capabilities can be used concurrently in the superclass they must also be
              able to be used concurrently in subclasses *)
           not
             ( can_concurrently_access_capabilities superclass class_defns superclass_cap1
                 superclass_cap2
             || can_concurrently_access_capabilities class_name class_defns
                  superclass_cap1 superclass_cap2 )
         then Ok ()
         else
           Error
             (Error.of_string
                (Fmt.str
                   "Type error: %s and %s can be used concurrently in %s but not in subclass %s@."
                   (string_of_cap superclass_cap1)
                   (string_of_cap superclass_cap2)
                   (Class_name.to_string superclass)
                   (Class_name.to_string class_name))))
       superclass_capabilities superclass_capabilities)

let type_subtyping class_defns (TClass (class_name, maybe_inherits, _, _, _)) =
  match maybe_inherits with
  | None            -> Ok ()
  | Some superclass -> type_subtyping_capabilities class_defns class_name superclass
