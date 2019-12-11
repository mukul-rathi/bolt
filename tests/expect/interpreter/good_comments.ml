open Core
open Print_execution

let%expect_test "Comments interspersed with code" =
  print_execution
    " 
    (* This is a comment - it should not be parsed *) 

    let x = 4 in (* Can occur after a line *)
    let y (*Or even midway*) = 5 in
    (* Or before *) x
    end
    (*
    Comments
    Can 
    Span 
    Multiple 
    Lines
    *)
    end
  " ;
  [%expect
    {|
    ----- Step 0 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 4); BIND(x); PUSH(Int: 5); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [  ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 1 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); PUSH(Int: 5); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 4 ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 2 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 5); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ x -> Int: 4 ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 3 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Int: 4 ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 4 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ y -> Int: 5 ], Env: [ x -> Int: 4 ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 5 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 4, Env: [ y -> Int: 5 ], Env: [ x -> Int: 4 ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 6 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP; SWAP; POP ]
       └──Stack: [ Env: [ y -> Int: 5 ], Value: Int: 4, Env: [ x -> Int: 4 ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 7 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP ]
       └──Stack: [ Value: Int: 4, Env: [ x -> Int: 4 ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 8 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP ]
       └──Stack: [ Env: [ x -> Int: 4 ], Value: Int: 4 ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 9 - OUTPUT STATE --------
    Threads:
    └──Thread: 1
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 4 ]
    Heap: [  ]
    ------------------------------------------
    Output: Int: 4 |}]
