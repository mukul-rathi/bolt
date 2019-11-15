open Core
open Lex_and_parse

let get_file_extension filename =
  String.split_on_chars filename ~on:['.'] |> List.last |> Option.value ~default:""

let bolt_file =
  let error_not_file filename =
    eprintf "'%s' is not a bolt file. Hint: use the .bolt extension\n%!" filename ;
    exit 1 in
  Command.Spec.Arg_type.create (fun filename ->
      match Sys.is_file filename with
      | `Yes ->
          if get_file_extension filename = "bolt" then filename
          else error_not_file filename
      | `No | `Unknown -> error_not_file filename)

let maybe_pprint_ast should_pprint_ast parsed_ast =
  if should_pprint_ast then pprint_ast Format.std_formatter parsed_ast else ()

let run_program filename should_pprint_ast () =
  parse_program filename
  |> function
  | Some parsed_ast -> maybe_pprint_ast should_pprint_ast parsed_ast | None -> ()

let command =
  Command.basic ~summary:"Run bolt programs"
    ~readme:(fun () -> "A list of execution options")
    Command.Let_syntax.(
      let%map_open should_pprint_ast =
        flag "-print-ast" no_arg ~doc:" Pretty print the AST of the program"
      and filename = anon (maybe_with_default "-" ("filename" %: bolt_file)) in
      run_program filename should_pprint_ast)

let () = Command.run ~version:"1.0" ~build_info:"RWO" command
