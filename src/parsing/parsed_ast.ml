open Ast.Ast_types

(* Possible types of executable expressions - note we pass in the location of the start
   token to provide useful debugging information - which line + position the parsing
   errors occurred *)
type expr =
  | Integer     of loc * int
  | Variable    of loc * Var_name.t
  | Lambda      of loc * Var_name.t * type_expr * expr
      (** argument_variable, argument_type and body expression of function *)
  | App         of loc * expr * expr
  | Seq         of loc * expr list
  | Let         of loc * Var_name.t * expr * expr
  | ObjField    of loc * Var_name.t * Field_name.t
  | Assign      of loc * Var_name.t * Field_name.t * expr
  | Constructor of loc * Class_name.t * constructor_arg list
  | Consume     of loc * expr
  | FinishAsync of loc * expr * expr * expr

and constructor_arg = ConstructorArg of Field_name.t * expr

(* Each bolt program defines the classes, followed by the traits, followed by the
   expression to execute. *)
type program = Prog of class_defn list * trait_defn list * expr

let rec exprs_equal expr1 expr2 =
  match expr1 with
  | Integer (_, i1)                                            -> ( match expr2 with
                                                                    | Integer (_, i2) ->
                                                                        i1 = i2
                                                                    | _               -> false
                                                                    )
  | Variable (_, var_name1)                                    -> (
    match expr2 with Variable (_, var_name2) -> var_name1 = var_name2 | _ -> false )
  | Lambda (_, arg_1, arg_type_1, body_1)                      -> (
    match expr2 with
    | Lambda (_, arg_2, arg_type_2, body_2) ->
        arg_1 = arg_2 && arg_type_1 = arg_type_2 && exprs_equal body_1 body_2
    | _                                     -> false )
  | App (_, func_1, arg_1)                                     -> (
    match expr2 with
    | App (_, func_2, arg_2) -> exprs_equal func_1 func_2 && exprs_equal arg_1 arg_2
    | _                      -> false )
  | Seq (_, exprs_1)                                           -> (
    match expr2 with
    | Seq (_, exprs_2) ->
        List.length exprs_1 = List.length exprs_2
        && List.for_all2 exprs_equal exprs_1 exprs_2
    | _                -> false )
  | Let (_, var_name_1, expr_to_sub_1, body_expr_1)            -> (
    match expr2 with
    | Let (_, var_name_2, expr_to_sub_2, body_expr_2) ->
        var_name_1 = var_name_2
        && exprs_equal expr_to_sub_1 expr_to_sub_2
        && exprs_equal body_expr_1 body_expr_2
    | _                                               -> false )
  | ObjField (_, var_name_1, field_name_1)                     -> (
    match expr2 with
    | ObjField (_, var_name_2, field_name_2) ->
        var_name_1 = var_name_2 && field_name_1 = field_name_2
    | _                                      -> false )
  | Assign (_, var_name_1, field_name_1, assigned_expr_1)      -> (
    match expr2 with
    | Assign (_, var_name_2, field_name_2, assigned_expr_2) ->
        var_name_1 = var_name_2 && field_name_1 = field_name_2
        && exprs_equal assigned_expr_1 assigned_expr_2
    | _                                                     -> false )
  | Constructor (_, class_name_1, constructor_args_1)          -> (
    match expr2 with
    | Constructor (_, class_name_2, constructor_args_2) ->
        class_name_1 = class_name_2
        && List.for_all2 constructor_args_equal constructor_args_1 constructor_args_2
    | _                                                 -> false )
  | Consume (_, expr_1)                                        -> (
    match expr2 with Consume (_, expr_2) -> exprs_equal expr_1 expr_2 | _ -> false )
  | FinishAsync (_, async_expr1_1, async_expr2_1, next_expr_1) -> (
    match expr2 with
    | FinishAsync (_, async_expr1_2, async_expr2_2, next_expr_2) ->
        List.for_all2 exprs_equal
          [async_expr1_1; async_expr2_1; next_expr_1]
          [async_expr1_2; async_expr2_2; next_expr_2]
    | _                                                          -> false )

and constructor_args_equal (ConstructorArg (field_1, expr_1))
    (ConstructorArg (field_2, expr_2)) =
  field_1 = field_2 && exprs_equal expr_1 expr_2

let pasts_equal (Prog (cls_defns_1, trt_defns_1, expr1))
    (Prog (cls_defns_2, trt_defns_2, expr2)) =
  cls_defns_1 = cls_defns_2 && trt_defns_1 = trt_defns_2 && exprs_equal expr1 expr2
