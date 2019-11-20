/* This is the specification for the parser */

%{
  open Ast_types
let get_loc = Parsing.symbol_start_pos 
%}

/* Token definitions */

%token  <int> INT
%token  <string> ID
%token  LPAREN
%token  RPAREN 
%token  LBRACE 
%token  RBRACE 
%token  COMMA 
%token  DOT 
%token  COLON 
%token  SEMICOLON 
%token  EQUAL 
%token  ASSIGN 
%token  ARROW 
%token  BEGIN 
%token  LET 
%token  IN 
%token  END 
%token  NEW 
%token  CONST 
%token  VAR 
%token  FUN 
%token  CONSUME 
%token  FINISH 
%token  ASYNC 
%token  CLASS 
%token  TRAIT 
%token  REQUIRE 
%token  LINEAR 
%token  THREAD 
%token  READ 
%token  TYPE_INT 
%token  NULL 
%token  EOF 


%start program
%type <Ast_types.program> program
%type <class_defn> class_defn
%type <trait_defn> trait_defn
%type <type_expr> type_expr
%type <require_field_defn> require_field_defn
%type <field_defn> field_defn
%type <cap_trait> cap_trait
%type <capability> capability
%type <mode> mode
%type <type_field> tfield
%type <expr> expr
%type <expr> simple_expr
%type <constructor_args> constructor_args

%%

/* Grammar production 
 * Note: $i refers to the i'th (non)terminal symbol in the rule*/

program: 
| list(class_defn) list(trait_defn) expr EOF {Prog($1, $2, $3)}

type_expr : 
| cap_trait {TECapTrait($1)}
| ID        {TEClass(Class_name.of_string $1)} 
| TYPE_INT       {TEInt} 

class_defn:
| CLASS ID EQUAL cap_trait LBRACE nonempty_list(field_defn) RBRACE {TClass( Class_name.of_string $2, $4, $6)}

trait_defn:
| capability TRAIT ID LBRACE nonempty_list(require_field_defn) RBRACE { TTrait( Trait_name.of_string $3, $1, $5)}
require_field_defn:
| REQUIRE field_defn {TRequire($2)}

field_defn:
| mode ID COLON tfield {TField($1, Field_name.of_string $2, $4)}

cap_trait:
| capability ID {TCapTrait($1, Trait_name.of_string $2)}

capability:
| LINEAR {Linear}
| THREAD {Thread}
| READ  {Read}
mode:
| CONST {MConst}
| VAR {MVar}


tfield:
| TYPE_INT {TFieldInt}

lambda:
| FUN ID COLON type_expr  ARROW expr  END { Lambda(get_loc(), Var_name.of_string $2, $4, $6)}
| LPAREN lambda RPAREN {$2}

simple_expr:
| NULL {Null(get_loc())} 
| INT {Integer(get_loc(), $1)}
| ID {Variable(get_loc(), Var_name.of_string $1)} 
| lambda { $1 }

expr:
| simple_expr { $1 }
| LET ID EQUAL expr  IN expr END {Let(get_loc(), Var_name.of_string $2, $4, $6)} 
| ID DOT ID {ObjField(get_loc(), Var_name.of_string $1, Field_name.of_string $3)}
| ID DOT ID ASSIGN expr {Assign(get_loc(), Var_name.of_string $1, Field_name.of_string $3, $5)}
| NEW ID {Constructor(get_loc(),  Class_name.of_string $2, [])}
| NEW ID LPAREN separated_list(COMMA, constructor_args) RPAREN {Constructor(get_loc(),  Class_name.of_string $2, $4 )}
| CONSUME ID {Consume(get_loc(),  Var_name.of_string $2)}
| FINISH LBRACE ASYNC LBRACE expr RBRACE ASYNC LBRACE expr RBRACE RBRACE SEMICOLON expr {FinishAsync(get_loc(), $5, $9, $13)}
| BEGIN separated_list(SEMICOLON, expr) END { Seq(get_loc(), $2)}
| simple_expr  expr  {App(get_loc(), $1, $2)} 


constructor_args:
| ID COLON simple_expr {ConstructorArgs( Field_name.of_string $1,$3)}

