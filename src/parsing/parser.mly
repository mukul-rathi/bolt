/* This is the specification for the parser */

%{
  [@@@coverage exclude_file]
  open Ast.Ast_types
  open Parsed_ast
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
%token PLUS
%token MINUS
%token MULT
%token DIV
%token REM
%token LESS_THAN
%token GREATER_THAN
%token AND
%token OR
%token EXCLAMATION_MARK
%token  LET 
%token  NEW 
%token  CONST 
%token  VAR 
%token  FUNCTION 
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
%token TYPE_BOOL
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token EOF 
%token WHILE
/* 
Define operators' precedence - listed from low -> high priority
Note here we only have one operator, but we could have a list
) 

+Associativity resolves shift-reduce conflicts between rule and token:
  - left = reduce
  - right = shift
  - nonassoc - raise syntax error
*/




%start program
%type <Parsed_ast.program> program
%type <class_defn> class_defn
%type <trait_defn> trait_defn
%type <function_defn> function_defn
%type <type_expr> type_expr
%type <require_field_defn> require_field_defn
%type <field_defn> field_defn
%type <cap_trait> cap_trait
%type <capability> capability
%type <mode> mode
%type <type_field> tfield
%type <expr> expr
%type <expr> simple_expr
%type <constructor_arg> constructor_arg
%%

/* Grammar production 
 * Note: $i refers to the i'th (non)terminal symbol in the rule*/

program: 
| list(class_defn) list(trait_defn) list(function_defn) expr EOF {Prog($1, $2, $3, $4)}

type_expr : 
| cap_trait {TECapTrait($1)}
| ID        {TEClass(Class_name.of_string $1)} 
| TYPE_INT       {TEInt} 
| TYPE_BOOL {TEBool}

class_defn:
| CLASS ID EQUAL cap_trait LBRACE nonempty_list(field_defn) list(method_defn) RBRACE {TClass( Class_name.of_string $2, $4, $6, $7)}

method_defn: 
| type_expr ID params  expr  {TFunction(Function_name.of_string $2, $1, $3, $4)}

trait_defn:
| capability TRAIT ID LBRACE nonempty_list(require_field_defn) RBRACE { TTrait( Trait_name.of_string $3, $1, $5)}
require_field_defn:
| REQUIRE field_defn {TRequire($2)}

field_defn:
| mode ID COLON tfield {TField($1, Field_name.of_string $2, $4)}

function_defn: 
| FUNCTION type_expr ID params expr  {TFunction(Function_name.of_string $3, $2, $4, $5)}

params:
| LPAREN separated_nonempty_list(COMMA,param) RPAREN {$2}
| LPAREN  RPAREN {[TVoid]}

param:
| type_expr ID {TParam($1, Var_name.of_string $2)}

cap_trait:
| capability ID {TCapTrait($1, Trait_name.of_string $2)}

capability:
| LINEAR {Linear}
| THREAD {Thread}
| READ  {Read}
mode:
| CONST {MConst}
| VAR {MVar}

bin_op:
| PLUS { BinOpPlus }
| MINUS { BinOpMinus }
| MULT { BinOpMult }
| DIV { BinOpIntDiv } 
| REM { BinOpRem }
| LESS_THAN { BinOpLessThan }
| LESS_THAN EQUAL { BinOpLessThanEq }
| GREATER_THAN { BinOpGreaterThan }
| GREATER_THAN EQUAL{ BinOpGreaterThanEq }
| AND {BinOpAnd}
| OR {BinOpOr}
| EQUAL EQUAL {BinOpEq}
| EXCLAMATION_MARK EQUAL {BinOpNotEq}

un_op:
| EXCLAMATION_MARK {UnOpNot}


tfield:
| TYPE_INT {TFieldInt}
| TYPE_BOOL {TFieldBool}


simple_expr:
| INT {Integer($startpos, $1)}
| ID {Variable($startpos, Var_name.of_string $1)} 
| TRUE {Boolean($startpos, true)}
| FALSE {Boolean($startpos, false)}

args:
| LPAREN RPAREN {[Unit($startpos)]}
| LPAREN separated_nonempty_list(COMMA, expr) RPAREN {$2}

expr:
| un_op expr  { UnOp($startpos, $1, $2) }
| LPAREN  expr bin_op expr RPAREN {BinOp($startpos, $3, $2, $4)}
| simple_expr { $1 }
| LET ID EQUAL expr  {Let($startpos, Var_name.of_string $2, $4)} 
| ID DOT ID {ObjField($startpos, Var_name.of_string $1, Field_name.of_string $3)}
| ID DOT ID ASSIGN expr {Assign($startpos, Var_name.of_string $1, Field_name.of_string $3, $5)}
| NEW ID {Constructor($startpos,  Class_name.of_string $2, [])}
| NEW ID LPAREN separated_list(COMMA, constructor_arg) RPAREN {Constructor($startpos, Class_name.of_string $2, $4 )}
| CONSUME ID {Consume($startpos, Variable($startpos, Var_name.of_string $2))}
| FINISH LBRACE ASYNC expr ASYNC expr RBRACE SEMICOLON expr {FinishAsync($startpos, $4, $6, $9)}
| LBRACE separated_list(SEMICOLON, expr) RBRACE { Block($startpos, $2)}
| ID  args {App($startpos, Function_name.of_string $1, $2)} 
| ID DOT ID args {ObjMethod($startpos, Var_name.of_string $1, Function_name.of_string $3, $4) }
| IF expr option(THEN) expr ELSE expr {If($startpos, $2, $4, $6)}
| WHILE expr expr {While($startpos, $2, $3)}


constructor_arg:
| ID COLON simple_expr {ConstructorArg( Field_name.of_string $1,$3)}

