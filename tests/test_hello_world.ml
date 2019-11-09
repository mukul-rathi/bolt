let test_something () = Alcotest.(check int) "same int" 1 (Hello_world.id 1)

let test_other_thing =
  QCheck.Test.make ~count:1000 ~name:"zero"
    QCheck.(int)
    (fun i -> Hello_world.zero i = 0)

let () =
  let foo = List.map QCheck_alcotest.to_alcotest [test_other_thing] in
  Alcotest.run "Utils"
    [ ("string-case", [Alcotest.test_case "Something" `Quick test_something])
    ; ("foo", foo) ]
