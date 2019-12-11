open Core
open Print_execution

let%expect_test "Simple linear class" =
  print_execution
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    let x = new Foo() in 
      x.f:= 5
    end
  " ;
  [%expect
    {|
    ----- Step 0 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ CONSTRUCTOR(Foo); BIND(x); PUSH(Int: 5); STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [  ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 1 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); PUSH(Int: 5); STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 2 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 5); STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 3 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 4 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Int: 5, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 5 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 6 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 7 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP ]
       └──Stack: [ Env: [ x -> Address: 1 ], Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 8 - OUTPUT STATE --------
    Threads:
    └──Thread: 1
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    Output: Int: 5 |}]

let%expect_test "Simple thread class" =
  print_execution
    " 
    class Foo = thread Bar {
      var f : int
    }
    thread trait Bar {
      require var f : int
    }
    let x = new Foo() in 
      x.f:= 5
    end
  " ;
  [%expect
    {|
    ----- Step 0 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ CONSTRUCTOR(Foo); BIND(x); PUSH(Int: 5); STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [  ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 1 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); PUSH(Int: 5); STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 2 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 5); STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 3 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 4 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Int: 5, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 5 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 6 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 7 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP ]
       └──Stack: [ Env: [ x -> Address: 1 ], Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 8 - OUTPUT STATE --------
    Threads:
    └──Thread: 1
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    Output: Int: 5 |}]

let%expect_test "Simple read class" =
  print_execution
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    let x = new Foo(f:5) in 
      x.f
    end
  " ;
  [%expect
    {|
    ----- Step 0 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 5); CONSTRUCTOR(Foo); HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [  ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 1 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ CONSTRUCTOR(Foo); HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Int: 5 ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 2 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 3 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 4 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 5 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_LOOKUP(f); SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 6 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 7 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP ]
       └──Stack: [ Env: [ x -> Address: 1 ], Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 8 - OUTPUT STATE --------
    Threads:
    └──Thread: 1
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    Output: Int: 5 |}]
