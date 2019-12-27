/* This is the specification for the parser */

%{
  [@@@coverage exclude_file]
  open Ast.Ast_types
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
%token  PLUS
%token  MINUS
%token  MULT
%token  DIV
%token  REM
%token  LESS_THAN
%token  GREATER_THAN
%token  AND
%token  OR
%token  EXCLAMATION_MARK
%token  LET 
%token  NEW 
%token  CONST 
%token  VAR 
%token  FUNCTION 
%token  CONSUME 
%token  FINISH 
%token  ASYNC 
%token  CLASS 
%token  REGION 
%token  LINEAR 
%token  THREAD 
%token  READ 
%token  SUBORDINATE 
%token  LOCKED 
%token  TYPE_INT 
%token  TYPE_BOOL
%token  TYPE_VOID
%token  TRUE
%token  FALSE
%token  IF
%token  ELSE
%token  EOF 
%token  FOR
%token  WHILE
%token  MAIN

%start program
%type <int list> program
%%

/* Grammar production 
 * Note: $i refers to the i'th (non)terminal symbol in the rule*/

program: 
| list(class_defn) list(function_defn) main_expr EOF {[]}


/* Productions related to class definitions */

class_defn:
| CLASS ID LBRACE REGION region list(field_defn) list(method_defn) RBRACE {}


/* Capabilities and Regions */
capability:
| LINEAR { }
| THREAD { }
| READ  { }
| SUBORDINATE {}
| LOCKED {}

region:
| region PLUS simple_region {}
| region MULT simple_region {}
| simple_region {}

simple_region:
| capability ID {}
| LPAREN region RPAREN {}


/* Field definitions */

mode:
| CONST {MConst}
| VAR {MVar}

field_defn:
| mode type_expr ID COLON nonempty_list(region) {}


/* Method and function definitions */

params:
| LPAREN separated_nonempty_list(COMMA,param) RPAREN {}
| LPAREN RPAREN {}

param:
| type_expr ID option(COLON) option(region) {}


method_defn: 
| type_expr ID params COLON region block_expr {}

function_defn: 
| FUNCTION type_expr ID params block_expr  {}


/* Types */

type_expr : 
| ID        {} 
| TYPE_INT  {} 
| TYPE_BOOL {}
| TYPE_VOID {}


/* Expressions */

main_expr:
| TYPE_VOID MAIN LPAREN RPAREN block_expr {}

block_expr:
| LBRACE separated_list(SEMICOLON, expr) RBRACE {}


/* Method / function arguments */
args:
| LPAREN RPAREN {}
| LPAREN separated_nonempty_list(COMMA, expr) RPAREN {}

constructor_arg:
| ID COLON expr {}


identifier:
| ID {}
| ID DOT ID {}

simple_expr:
| LPAREN expr RPAREN {}
| INT {}
| TRUE { }
| FALSE { }
| identifier {}

expr:
| op_expr  {}
/*  Creating / reassigning references */
| NEW ID LPAREN separated_list(COMMA, constructor_arg) RPAREN {}
| LET ID EQUAL expr  {} 
| identifier ASSIGN expr {}
| CONSUME identifier {}
/* Function / Method Application */
| ID DOT ID args {}
| ID  args { } 
/* Control flow */
| IF expr block_expr ELSE expr {}
| WHILE expr block_expr {}
| FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN block_expr {}
/* Async expression */
| FINISH LBRACE separated_list(SEMICOLON, async_expr) expr RBRACE {}

async_expr:
| ASYNC block_expr {}


/* Operator expressions */

op_expr:
| un_op expr {}
| bin_op_expr {}

un_op:
| EXCLAMATION_MARK {}
| MINUS {}

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


bin_op_expr:
| bin_op_expr bin_op simple_expr {}
| simple_expr {}
