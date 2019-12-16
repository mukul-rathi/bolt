open Core
open Print_typed_ast

let%expect_test "Consume variable" =
  print_typed_ast
    " 
    class Foo = linear Bar {
      const f : int
      const g : int  
      const h : int

    }
    class Choco = thread Late {
      const f : int
    }
    class Bana = read Na {
      const f : int
    }
    thread trait Late {
      require const f : int
    }
    read trait Na {
      require const f : int
    }
    linear trait Bar {
      require const f : int
      require const g : int  
      require const h : int
    }
    {
      let x = new Foo(f:4, g:5, h:6) in
        let y = consume x in (* Consume linear variable *)
          let z = 5 in
            let w = consume z in (* Can consume an int *)
              y.h
            end
          end
        end
      end ;
      let x = new Choco(f:5) in
        let y = consume x in 
          y
        end
      end;
        let x = new Bana(f:5) in
        let y = consume x in 
          y
        end
      end
    }
  " ;
  [%expect
    {|
    Program
    └──Class: Foo
       └──CapTrait: Bar
          └──Cap: Linear
       └──Field Defn: f
          └──Mode: Const
          └──TField: Int
       └──Field Defn: g
          └──Mode: Const
          └──TField: Int
       └──Field Defn: h
          └──Mode: Const
          └──TField: Int
    └──Class: Choco
       └──CapTrait: Late
          └──Cap: Thread
       └──Field Defn: f
          └──Mode: Const
          └──TField: Int
    └──Class: Bana
       └──CapTrait: Na
          └──Cap: Read
       └──Field Defn: f
          └──Mode: Const
          └──TField: Int
    └──Trait: Late
       └──Cap: Thread
       └──Require
          └──Field Defn: f
             └──Mode: Const
             └──TField: Int
    └──Trait: Na
       └──Cap: Read
       └──Require
          └──Field Defn: f
             └──Mode: Const
             └──TField: Int
    └──Trait: Bar
       └──Cap: Linear
       └──Require
          └──Field Defn: f
             └──Mode: Const
             └──TField: Int
       └──Require
          └──Field Defn: g
             └──Mode: Const
             └──TField: Int
       └──Require
          └──Field Defn: h
             └──Mode: Const
             └──TField: Int
    └──Expr: Block
       └──Type expr: Class: Bana
       └──Expr: Let var: x
          └──Type expr: Int
          └──Expr: Constructor for: Foo
             └──Type expr: Class: Foo
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:4
             └── Field: g
                └──Type expr: Int
                └──Expr: Int:5
             └── Field: h
                └──Type expr: Int
                └──Expr: Int:6
          └──Expr: Let var: y
             └──Type expr: Int
             └──Expr: Consume
                └──Type expr: Class: Foo
                └──Expr: Variable: x
                   └──Type expr: Class: Foo
             └──Expr: Let var: z
                └──Type expr: Int
                └──Expr: Int:5
                └──Expr: Let var: w
                   └──Type expr: Int
                   └──Expr: Consume
                      └──Type expr: Int
                      └──Expr: Variable: z
                         └──Type expr: Int
                   └──Expr: Objfield: (Class: Foo) y.h
                      └──Type expr: Int
       └──Expr: Let var: x
          └──Type expr: Class: Choco
          └──Expr: Constructor for: Choco
             └──Type expr: Class: Choco
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:5
          └──Expr: Let var: y
             └──Type expr: Class: Choco
             └──Expr: Consume
                └──Type expr: Class: Choco
                └──Expr: Variable: x
                   └──Type expr: Class: Choco
             └──Expr: Variable: y
                └──Type expr: Class: Choco
       └──Expr: Let var: x
          └──Type expr: Class: Bana
          └──Expr: Constructor for: Bana
             └──Type expr: Class: Bana
             └── Field: f
                └──Type expr: Int
                └──Expr: Int:5
          └──Expr: Let var: y
             └──Type expr: Class: Bana
             └──Expr: Consume
                └──Type expr: Class: Bana
                └──Expr: Variable: x
                   └──Type expr: Class: Bana
             └──Expr: Variable: y
                └──Type expr: Class: Bana |}]
