(* Count instantiations of type parameters for generic classes and polymorphic functions *)

open Core
open Desugar_env
open Typing

let rec add_instantiation class_defns concrete_type class_name class_insts =
  List.find ~f:(fun (name, _) -> class_name = name) class_insts
  |> (function
       | None                     -> (class_name, [concrete_type])
       | Some (_, concrete_types) -> (class_name, concrete_type :: concrete_types))
  |> fun class_inst ->
  List.filter ~f:(fun (name, _) -> not (class_name = name)) class_insts
  |> fun other_class_insts ->
  class_inst :: other_class_insts
  |> fun updated_class_insts ->
  maybe_get_superclass class_name class_defns
  |> function
  | Some superclass ->
      add_instantiation class_defns concrete_type superclass updated_class_insts
  | None            -> updated_class_insts

let rec count_generics_instantiations_expr class_defns expr class_insts =
  match expr with
  | Typed_ast.Integer _ | Typed_ast.Boolean _ | Typed_ast.Identifier _ -> class_insts
  | Typed_ast.BlockExpr (_, block_expr) ->
      count_generics_instantiations_block_expr class_defns block_expr class_insts
  | Typed_ast.Constructor (_, class_name, maybe_type_param, constructor_args) ->
      ( match maybe_type_param with
      | Some TEGeneric  -> class_insts (* only consider concrete type params *)
      | Some type_param -> add_instantiation class_defns type_param class_name class_insts
      | None            -> class_insts )
      |> fun updated_class_insts ->
      List.fold ~init:updated_class_insts
        ~f:(fun acc_class_insts (Typed_ast.ConstructorArg (_, _, arg_expr)) ->
          count_generics_instantiations_expr class_defns arg_expr acc_class_insts)
        constructor_args
  | Typed_ast.Let (_, _, _, bound_expr) ->
      count_generics_instantiations_expr class_defns bound_expr class_insts
  | Typed_ast.Assign (_, _, _, assigned_expr) ->
      count_generics_instantiations_expr class_defns assigned_expr class_insts
  | Typed_ast.Consume _ -> class_insts
  | Typed_ast.MethodApp (_, _, _, _, _, _, _, args) ->
      List.fold ~init:class_insts
        ~f:(fun acc_class_insts arg_expr ->
          count_generics_instantiations_expr class_defns arg_expr acc_class_insts)
        args
  | Typed_ast.FunctionApp (_, _, _, _, args) ->
      List.fold ~init:class_insts
        ~f:(fun acc_class_insts arg_expr ->
          count_generics_instantiations_expr class_defns arg_expr acc_class_insts)
        args
  | Typed_ast.Printf (_, _, args) ->
      List.fold ~init:class_insts
        ~f:(fun acc_class_insts arg_expr ->
          count_generics_instantiations_expr class_defns arg_expr acc_class_insts)
        args
  | Typed_ast.FinishAsync (_, _, async_exprs, curr_thread_expr) ->
      count_generics_instantiations_block_expr class_defns curr_thread_expr class_insts
      |> fun updated_class_insts ->
      List.fold ~init:updated_class_insts
        ~f:(fun acc_class_insts (Typed_ast.AsyncExpr async_expr) ->
          count_generics_instantiations_block_expr class_defns async_expr acc_class_insts)
        async_exprs
  | Typed_ast.If (_, _, cond_expr, then_expr, else_expr) ->
      count_generics_instantiations_expr class_defns cond_expr class_insts
      |> fun updated_class_insts ->
      List.fold ~init:updated_class_insts
        ~f:(fun acc_class_insts branch_expr ->
          count_generics_instantiations_block_expr class_defns branch_expr acc_class_insts)
        [then_expr; else_expr]
  | Typed_ast.While (_, cond_expr, loop_expr) ->
      count_generics_instantiations_expr class_defns cond_expr class_insts
      |> fun updated_class_insts ->
      count_generics_instantiations_block_expr class_defns loop_expr updated_class_insts
  | Typed_ast.BinOp (_, _, _, expr1, expr2) ->
      List.fold ~init:class_insts
        ~f:(fun acc_class_insts op_expr ->
          count_generics_instantiations_expr class_defns op_expr acc_class_insts)
        [expr1; expr2]
  | Typed_ast.UnOp (_, _, _, op_expr) ->
      count_generics_instantiations_expr class_defns op_expr class_insts

and count_generics_instantiations_block_expr class_defns (Typed_ast.Block (_, _, exprs))
    class_insts =
  List.fold ~init:class_insts
    ~f:(fun acc_class_insts expr ->
      count_generics_instantiations_expr class_defns expr acc_class_insts)
    exprs

let count_generics_instantiations_function_defn class_defns
    (Typed_ast.TFunction (_, _, _, _, body_expr)) class_insts =
  count_generics_instantiations_block_expr class_defns body_expr class_insts

let count_generics_instantiations_class_defn class_defns
    (Typed_ast.TClass (_, _, _, _, _, method_defns)) class_insts =
  List.fold ~init:class_insts
    ~f:(fun acc_class_insts (Typed_ast.TMethod (_, _, _, _, _, body_expr)) ->
      count_generics_instantiations_block_expr class_defns body_expr acc_class_insts)
    method_defns

let count_generics_instantiations_program
    (Typed_ast.Prog (class_defns, function_defns, main_expr)) =
  count_generics_instantiations_block_expr class_defns main_expr []
  |> fun class_insts ->
  List.fold ~init:class_insts
    ~f:(fun acc_class_insts class_defn ->
      count_generics_instantiations_class_defn class_defns class_defn acc_class_insts)
    class_defns
  |> fun updated_class_ints ->
  List.fold ~init:updated_class_ints
    ~f:(fun acc_class_insts function_defn ->
      count_generics_instantiations_function_defn class_defns function_defn
        acc_class_insts)
    function_defns
