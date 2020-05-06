open Ast.Ast_types
open Core
open Desugaring.Desugared_ast

(* Name mangles method name so doesn't clash with other methods/functions *)
let ir_gen_method_name meth_name class_name =
  Fmt.str "_%s_%s" (Class_name.to_string class_name) (Method_name.to_string meth_name)

let get_class_defn class_name class_defns =
  let matching_class_defns =
    List.filter ~f:(fun (TClass (name, _, _, _, _)) -> class_name = name) class_defns
  in
  (* This should never throw an exception since we've checked this property in earlier
     type-checking stages of the pipeline *)
  List.hd_exn matching_class_defns

let rec get_class_fields class_name class_defns =
  get_class_defn class_name class_defns
  |> fun (TClass (_, maybe_inherits, _, field_defns, _)) ->
  ( match maybe_inherits with
  | Some super_class -> get_class_fields super_class class_defns
  | None             -> [] )
  |> fun superclass_fields -> List.concat [superclass_fields; field_defns]

let rec replace_overridden_method_annots class_name class_methods = function
  | [] -> []
  | (superclass_annot, superclass_method) :: superclass_methods ->
      ( if List.exists ~f:(fun meth_name -> superclass_method = meth_name) class_methods
      then (class_name, superclass_method)
      else (superclass_annot, superclass_method) )
      |> fun maybe_replaced_annot ->
      maybe_replaced_annot
      :: replace_overridden_method_annots class_name class_methods superclass_methods

let rec get_class_annotated_methods class_name class_defns =
  (* get a list of methods in the class, annotated with the class they came from.
     Superclass methods come first, followed by subclasses. We replace any overridden
     methods with their subclass's annotation *)
  get_class_defn class_name class_defns
  |> fun (TClass (_, maybe_inherits, _, _, method_defns)) ->
  List.map ~f:(fun (TMethod (meth_name, _, _, _, _, _)) -> meth_name) method_defns
  |> fun class_methods ->
  ( match maybe_inherits with
  | Some superclass -> get_class_annotated_methods superclass class_defns
  | None            -> [] )
  |> fun superclass_methods ->
  replace_overridden_method_annots class_name class_methods superclass_methods
  |> fun replaced_overridden_methods ->
  List.filter_map
    ~f:(fun meth_name ->
      if List.exists ~f:(fun (_, name) -> meth_name = name) replaced_overridden_methods
      then None
      else Some (class_name, meth_name))
    class_methods
  |> fun class_not_overridden_methods ->
  List.concat [replaced_overridden_methods; class_not_overridden_methods]

let ir_gen_vtable_method_index method_name class_name class_defns =
  get_class_annotated_methods class_name class_defns
  |> fun class_annotated_methods ->
  List.find_mapi_exn
    ~f:(fun index (_, name) -> if name = method_name then Some index else None)
    class_annotated_methods

let ir_gen_vtable class_name class_defns =
  get_class_annotated_methods class_name class_defns
  |> fun class_annotated_methods ->
  List.map
    ~f:(fun (class_annot, meth_name) -> ir_gen_method_name meth_name class_annot)
    class_annotated_methods

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
  get_class_fields class_name class_defns
  |> fun field_defns ->
  (* first two fields in a class are reserved for vtable, threadID, typeOfLock and
     lockCount *)
  get_class_field_index class_name field_defns field_name 4
