open Ast.Ast_types
open Typing
open Core

let name_mangle_param_types param_types =
  String.concat
    (List.map
       ~f:(function
         | TEGeneric               -> raise
                                        (NotDesugaredGenericType "Desugaring - overloading")
         | TEVoid                  -> "v"
         | TEInt                   -> "i"
         | TEBool                  -> "b"
         | TEClass (class_name, _) ->
             let class_name_str = Class_name.to_string class_name in
             Fmt.str "%d%s" (String.length class_name_str) class_name_str)
       param_types)

let name_mangle_overloaded_method meth_name param_types =
  Method_name.of_string
    (Fmt.str "_%s%s"
       (Method_name.to_string meth_name)
       (name_mangle_param_types param_types))

let name_mangle_if_overloaded_function function_defns func_name param_types =
  List.filter
    ~f:(fun (Typed_ast.TFunction (name, _, _, _, _)) -> name = func_name)
    function_defns
  |> fun matching_function_defns ->
  if List.length matching_function_defns > 1 then
    (* Function is overloaded so name mangle *)
    Function_name.of_string
      (Fmt.str "_%s%s"
         (Function_name.to_string func_name)
         (name_mangle_param_types param_types))
  else func_name
