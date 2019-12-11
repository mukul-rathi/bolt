open Core
open Print_execution

let%expect_test "Equivalent constructor expressions" =
  print_execution
    " 
    class Foo = linear Bar {
      var f : int
    }
    linear trait Bar {
      require var f : int
    }
    let x = new Foo() in 
      let y = new Foo (* This is equivalent *)
        in x.f := 5
      end
    end
  " ;
  [%expect
    {|
    ----- Step 0 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ CONSTRUCTOR(Foo); BIND(x); CONSTRUCTOR(Foo); BIND(y); PUSH(Int: 5); STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP ]
       └──Stack: [  ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 1 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); CONSTRUCTOR(Foo); BIND(y); PUSH(Int: 5); STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 2 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ CONSTRUCTOR(Foo); BIND(y); PUSH(Int: 5); STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 3 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(y); PUSH(Int: 5); STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 2, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } }, 2 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 4 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 5); STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ y -> Address: 2 ], Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } }, 2 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 5 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ y -> Address: 2 ], Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } }, 2 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 6 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_SET(f); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Int: 5, Env: [ y -> Address: 2 ], Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } }, 2 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 7 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ y -> Address: 2 ], Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } }, 2 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 8 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ y -> Address: 2 ], Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } }, 2 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 9 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP; SWAP; POP ]
       └──Stack: [ Env: [ y -> Address: 2 ], Value: Int: 5, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } }, 2 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 10 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } }, 2 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 11 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP ]
       └──Stack: [ Env: [ x -> Address: 1 ], Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } }, 2 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 12 - OUTPUT STATE --------
    Threads:
    └──Thread: 1
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } }, 2 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    Output: Int: 5 |}]

let%expect_test "Constructor with multiple args" =
  print_execution
    " 
    class Foo = linear Bar {
      const f : int
      const g : int  
      const h : int
    }
    linear trait Bar {
      require const f : int
      require const g : int  
      require const h : int
    }
    let x = new Foo(f:4, g:5, h:6) in 
      x
    end
  " ;
  [%expect
    {|
    ----- Step 0 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 4); PUSH(Int: 5); PUSH(Int: 6); CONSTRUCTOR(Foo); HEAP_FIELD_SET(h); HEAP_FIELD_SET(g); HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); SWAP; POP ]
       └──Stack: [  ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 1 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 5); PUSH(Int: 6); CONSTRUCTOR(Foo); HEAP_FIELD_SET(h); HEAP_FIELD_SET(g); HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); SWAP; POP ]
       └──Stack: [ Value: Int: 4 ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 2 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 6); CONSTRUCTOR(Foo); HEAP_FIELD_SET(h); HEAP_FIELD_SET(g); HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); SWAP; POP ]
       └──Stack: [ Value: Int: 5, Value: Int: 4 ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 3 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ CONSTRUCTOR(Foo); HEAP_FIELD_SET(h); HEAP_FIELD_SET(g); HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); SWAP; POP ]
       └──Stack: [ Value: Int: 6, Value: Int: 5, Value: Int: 4 ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 4 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_SET(h); HEAP_FIELD_SET(g); HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Int: 6, Value: Int: 5, Value: Int: 4 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 5 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_SET(g); HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Int: 5, Value: Int: 4 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { h: Int: 6 } } ]
    ------------------------------------------
    ----- Step 6 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_SET(f); BIND(x); STACK_LOOKUP(x); SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Int: 4 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { h: Int: 6; g: Int: 5 } } ]
    ------------------------------------------
    ----- Step 7 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); STACK_LOOKUP(x); SWAP; POP ]
       └──Stack: [ Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { h: Int: 6; g: Int: 5; f: Int: 4 } } ]
    ------------------------------------------
    ----- Step 8 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); SWAP; POP ]
       └──Stack: [ Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { h: Int: 6; g: Int: 5; f: Int: 4 } } ]
    ------------------------------------------
    ----- Step 9 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ x -> Address: 1 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { h: Int: 6; g: Int: 5; f: Int: 4 } } ]
    ------------------------------------------
    ----- Step 10 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP ]
       └──Stack: [ Env: [ x -> Address: 1 ], Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { h: Int: 6; g: Int: 5; f: Int: 4 } } ]
    ------------------------------------------
    ----- Step 11 - OUTPUT STATE --------
    Threads:
    └──Thread: 1
       └──Instructions: [  ]
       └──Stack: [ Value: Address: 1 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { h: Int: 6; g: Int: 5; f: Int: 4 } } ]
    ------------------------------------------
    Output: Address: 1 |}]
