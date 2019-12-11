open Core
open Print_execution

let%expect_test "Alias a linear variable" =
  print_execution
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    let x = new Foo() in 
      let y = x in (* cannot alias linear reference *)
        x
      end
    end
  " ;
  [%expect
    {|
    ----- Step 0 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ CONSTRUCTOR(Foo); BIND(x); STACK_LOOKUP(x); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [  ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 1 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); STACK_LOOKUP(x); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 2 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 3 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 4 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ y -> Address: 1 ], Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 5 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ y -> Address: 1 ], Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 6 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP; SWAP; POP ]
       └──Stack: [ Env: [ y -> Address: 1 ], Value: Address: 1, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 7 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 8 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP ]
       └──Stack: [ Env: [ x -> Address: 1 ], Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 9 - OUTPUT STATE --------
    Threads:
    └──Thread: 1
       └──Instructions: [  ]
       └──Stack: [ Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    Output: Address: 1 |}]

let%expect_test "Alias a field of linear variable" =
  print_execution
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    let x = new Foo(f:5) in 
      let y = x.f in (* cannot alias field of a linear reference *)
        x
      end
    end
  " ;
  [%expect
    {|
    ----- Step 0 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 5); CONSTRUCTOR(Foo); HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [  ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 1 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ CONSTRUCTOR(Foo); HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 5 ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 2 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 3 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 4 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 5 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_LOOKUP(f); BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 6 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(y); STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 7 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 8 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 9 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP; SWAP; POP ]
       └──Stack: [ Env: [ y -> Int: 5 ], Value: Address: 1, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 10 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 11 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP ]
       └──Stack: [ Env: [ x -> Address: 1 ], Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 12 - OUTPUT STATE --------
    Threads:
    └──Thread: 1
       └──Instructions: [  ]
       └──Stack: [ Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    Output: Address: 1 |}]
