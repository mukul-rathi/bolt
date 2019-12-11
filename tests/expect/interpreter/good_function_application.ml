open Core
open Print_execution

let%expect_test "Lambda application" =
  print_execution " 
    (fun x : int -> x end) 4
  " ;
  [%expect
    {|
    ----- Step 0 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ MK_CLOSURE(BIND(x); STACK_LOOKUP(x); SWAP; POP); PUSH(Int: 4); APPLY; SWAP; POP ]
       └──Stack: [  ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 1 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 4); APPLY; SWAP; POP ]
       └──Stack: [ Value: Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 2 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ APPLY; SWAP; POP ]
       └──Stack: [ Value: Int: 4, Value: Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 3 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 4, Env: [  ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 4 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ x -> Int: 4 ], Env: [  ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 5 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 4, Env: [ x -> Int: 4 ], Env: [  ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 6 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP; SWAP; POP ]
       └──Stack: [ Env: [ x -> Int: 4 ], Value: Int: 4, Env: [  ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 7 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP ]
       └──Stack: [ Value: Int: 4, Env: [  ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 8 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP ]
       └──Stack: [ Env: [  ], Value: Int: 4 ]
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

let%expect_test "Function application" =
  print_execution " 
    let f = fun x :int -> x end 
    in f 4
    end
  " ;
  [%expect
    {|
    ----- Step 0 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ MK_CLOSURE(BIND(x); STACK_LOOKUP(x); SWAP; POP); BIND(f); STACK_LOOKUP(f); PUSH(Int: 4); APPLY; SWAP; POP; SWAP; POP ]
       └──Stack: [  ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 1 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(f); STACK_LOOKUP(f); PUSH(Int: 4); APPLY; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 2 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(f); PUSH(Int: 4); APPLY; SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ f -> Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 3 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 4); APPLY; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]), Env: [ f -> Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 4 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ APPLY; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 4, Value: Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]), Env: [ f -> Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 5 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); STACK_LOOKUP(x); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 4, Env: [  ], Env: [ f -> Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 6 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ x -> Int: 4 ], Env: [  ], Env: [ f -> Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 7 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 4, Env: [ x -> Int: 4 ], Env: [  ], Env: [ f -> Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 8 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ x -> Int: 4 ], Value: Int: 4, Env: [  ], Env: [ f -> Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 9 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 4, Env: [  ], Env: [ f -> Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 10 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP; SWAP; POP ]
       └──Stack: [ Env: [  ], Value: Int: 4, Env: [ f -> Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 11 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP ]
       └──Stack: [ Value: Int: 4, Env: [ f -> Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 12 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP ]
       └──Stack: [ Env: [ f -> Closure: ( Body: [ BIND(x); STACK_LOOKUP(x); SWAP; POP
      ] Env: [  ]) ], Value: Int: 4 ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 13 - OUTPUT STATE --------
    Threads:
    └──Thread: 1
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 4 ]
    Heap: [  ]
    ------------------------------------------
    Output: Int: 4 |}]
