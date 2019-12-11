open Core
open Print_execution

let%expect_test "Immutable refs in multiple threads" =
  print_execution
    " 
    class Foo = read Bar {
      const f : int
    }
    read trait Bar {
      require const f : int
    }
    let x = 6 in 
      let x = new Foo(f:5) in  (* shadowing over here *)
        let y = 5 in 
        finish{
          async {
            begin
            x ;
            y
            end
          }
          async{
            begin
            x ;
            y
            end
          }
        } ;
        x.f
        end
      end
    end
  " ;
  [%expect
    {|
    ----- Step 0 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 6); BIND(x); PUSH(Int: 5); CONSTRUCTOR(Foo); HEAP_FIELD_SET(f); BIND(x); PUSH(Int: 5); BIND(y); SPAWN [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y) ]; STACK_LOOKUP(x); POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [  ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 1 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); PUSH(Int: 5); CONSTRUCTOR(Foo); HEAP_FIELD_SET(f); BIND(x); PUSH(Int: 5); BIND(y); SPAWN [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y) ]; STACK_LOOKUP(x); POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 6 ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 2 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 5); CONSTRUCTOR(Foo); HEAP_FIELD_SET(f); BIND(x); PUSH(Int: 5); BIND(y); SPAWN [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y) ]; STACK_LOOKUP(x); POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ x -> Int: 6 ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 3 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ CONSTRUCTOR(Foo); HEAP_FIELD_SET(f); BIND(x); PUSH(Int: 5); BIND(y); SPAWN [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y) ]; STACK_LOOKUP(x); POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Int: 6 ] ]
    Heap: [  ]
    ------------------------------------------
    ----- Step 4 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_SET(f); BIND(x); PUSH(Int: 5); BIND(y); SPAWN [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y) ]; STACK_LOOKUP(x); POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Int: 5, Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: {  } } ]
    ------------------------------------------
    ----- Step 5 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(x); PUSH(Int: 5); BIND(y); SPAWN [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y) ]; STACK_LOOKUP(x); POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 6 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ PUSH(Int: 5); BIND(y); SPAWN [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y) ]; STACK_LOOKUP(x); POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 7 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BIND(y); SPAWN [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y) ]; STACK_LOOKUP(x); POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 8 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SPAWN [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y) ]; STACK_LOOKUP(x); POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 9 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Thread ID: 2, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    └──Thread: 2
       └──Instructions: [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y) ]
       └──Stack: [ Env: [ x -> Address: 1, y -> Int: 5 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 10 - scheduled thread : 2-----
    Threads:
    └──Thread: 2
       └──Instructions: [ STACK_LOOKUP(x); POP; STACK_LOOKUP(y) ]
       └──Stack: [ Env: [ x -> Address: 1, y -> Int: 5 ] ]
    └──Thread: 1
       └──Instructions: [ POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Thread ID: 2, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 11 - scheduled thread : 2-----
    Threads:
    └──Thread: 2
       └──Instructions: [ POP; STACK_LOOKUP(y) ]
       └──Stack: [ Value: Address: 1, Env: [ x -> Address: 1, y -> Int: 5 ] ]
    └──Thread: 1
       └──Instructions: [ POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Thread ID: 2, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 12 - scheduled thread : 2-----
    Threads:
    └──Thread: 2
       └──Instructions: [ STACK_LOOKUP(y) ]
       └──Stack: [ Env: [ x -> Address: 1, y -> Int: 5 ] ]
    └──Thread: 1
       └──Instructions: [ POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Thread ID: 2, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 13 - scheduled thread : 2-----
    Threads:
    └──Thread: 2
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1, y -> Int: 5 ] ]
    └──Thread: 1
       └──Instructions: [ POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Thread ID: 2, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 14 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP; STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Value: Thread ID: 2, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    └──Thread: 2
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1, y -> Int: 5 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 15 - scheduled thread : 2-----
    Threads:
    └──Thread: 2
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1, y -> Int: 5 ] ]
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Thread ID: 2, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 16 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(y); POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Thread ID: 2, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    └──Thread: 2
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1, y -> Int: 5 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 17 - scheduled thread : 2-----
    Threads:
    └──Thread: 2
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1, y -> Int: 5 ] ]
    └──Thread: 1
       └──Instructions: [ POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 5, Value: Thread ID: 2, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 18 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP; BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 5, Value: Thread ID: 2, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    └──Thread: 2
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1, y -> Int: 5 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 19 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ BLOCKED; STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Thread ID: 2, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    └──Thread: 2
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1, y -> Int: 5 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 20 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ STACK_LOOKUP(x); HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 21 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ HEAP_FIELD_LOOKUP(f); SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Address: 1, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 22 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ y -> Int: 5 ], Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 23 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP; SWAP; POP; SWAP; POP ]
       └──Stack: [ Env: [ y -> Int: 5 ], Value: Int: 5, Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 24 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP; SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Address: 1 ], Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 25 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP; SWAP; POP ]
       └──Stack: [ Env: [ x -> Address: 1 ], Value: Int: 5, Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 26 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ SWAP; POP ]
       └──Stack: [ Value: Int: 5, Env: [ x -> Int: 6 ] ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 27 - scheduled thread : 1-----
    Threads:
    └──Thread: 1
       └──Instructions: [ POP ]
       └──Stack: [ Env: [ x -> Int: 6 ], Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    ----- Step 28 - OUTPUT STATE --------
    Threads:
    └──Thread: 1
       └──Instructions: [  ]
       └──Stack: [ Value: Int: 5 ]
    Heap: [ 1 -> { Class_name: Foo, Fields: { f: Int: 5 } } ]
    ------------------------------------------
    Output: Int: 5 |}]
