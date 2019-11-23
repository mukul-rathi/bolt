open Core
open Result
open Type_core_lang
open Type_data_race

let type_check_program program ~allow_data_races =
  type_core_lang program
  >>= fun typed_program ->
  (if not allow_data_races then type_data_race typed_program else Ok ())
  >>| fun () -> typed_program

let pprint_typed_ast ppf (prog : Typed_ast.program) = Pprint_tast.pprint_program ppf prog
