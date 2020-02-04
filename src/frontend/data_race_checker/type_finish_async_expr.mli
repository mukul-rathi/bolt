(** Type checks the finish-async expression - removes thread capabilities from identifiers
    accessed in a different thread to their creation, and removes linear capabilities from
    variables accessed in more than one thread *)

val type_finish_async_expr : Data_race_checker_ast.expr -> unit
val type_finish_async_block_expr : Data_race_checker_ast.block_expr -> unit
