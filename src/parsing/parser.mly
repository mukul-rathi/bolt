/* This is the specification for the parser */

%{

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


/* 
Define operators' precedence - listed from low -> high priority
Note here we only have one operator, but we could have a list
) 

Associativity resolves shift-reduce conflicts between rule and token:
  - left = reduce
  - right = shift
  - nonassoc - raise syntax error
*/

%right ARROW /* fun x -> e  */ 


%start program
%type <Ast_types.program option> program
%type <Ast_types.class_defn> class_defn
%type <Ast_types.trait_defn> trait_defn
%type <Ast_types.type_expr> type_expr
%type <Ast_types.require_field_defn> require_field_defn
%type <Ast_types.field_defn> field_defn
%type <Ast_types.cap_trait> cap_trait
%type <Ast_types.capability> capability
%type <Ast_types.mode> mode
%type <Ast_types.type_field> tfield
%type <Ast_types.expr> expr
%type <Ast_types.expr> simple_expr
%type <Ast_types.constructor_args> constructor_args

%%

/* Grammar production 
 * Note: $i refers to the i'th (non)terminal symbol in the rule*/

program: 
| list(class_defn) list(trait_defn) expr EOF {Some(Ast_types.Prog($1, $2, $3))}
| EOF {None}

type_expr : 
| cap_trait {Ast_types.TECapTrait($1)}
| ID        {Ast_types.TEClass($1)} 
| TYPE_INT       {Ast_types.TEInt} 
| type_expr ARROW type_expr {TEFun($1, $3)} 

class_defn:
| CLASS ID EQUAL cap_trait LBRACE nonempty_list(field_defn) RBRACE {Ast_types.TClass($2, $4, $6)}

trait_defn:
| capability TRAIT ID LBRACE nonempty_list(require_field_defn) RBRACE { Ast_types.TTrait($3, $1, $5)}
require_field_defn:
| REQUIRE field_defn {Ast_types.TRequire($2)}

field_defn:
| mode ID COLON tfield {Ast_types.TField($1, $2, $4)}

cap_trait:
| capability ID {Ast_types.TCapTrait($1, $2)}

capability:
| LINEAR {Ast_types.Linear}
| THREAD {Ast_types.Thread}
| READ  {Ast_types.Read}
mode:
| CONST {Ast_types.MConst}
| VAR {Ast_types.MVar}


tfield:
| TYPE_INT {Ast_types.TFieldInt}

lambda:
| FUN ID COLON type_expr  ARROW expr  END { Ast_types.Lambda(get_loc(), $2, $4, $6)} /**/
| LPAREN lambda RPAREN {$2}

simple_expr:
| NULL {Ast_types.Null(get_loc())} /**/
| INT {Ast_types.Integer(get_loc(), $1)} /**/
| ID {Ast_types.Variable(get_loc(), $1)} 
| lambda { $1 }

expr:
| simple_expr { $1 }
| LET ID COLON type_expr EQUAL expr  IN expr END {Ast_types.Let(get_loc(), $2, $4, $6, $8)} /**/
| ID DOT ID {Ast_types.ObjField(get_loc(),$1, $3)}
| ID DOT ID ASSIGN expr {Ast_types.Assign(get_loc(),$1, $3, $5)}
| NEW ID {Ast_types.Constructor(get_loc(), $2, [])}
| NEW ID LPAREN separated_list(COMMA, constructor_args) RPAREN {Ast_types.Constructor(get_loc(), $2, $4 )}
| CONSUME ID {Ast_types.Consume(get_loc(), $2)} /**/
| FINISH LBRACE ASYNC LBRACE expr RBRACE ASYNC LBRACE expr RBRACE RBRACE SEMICOLON expr {Ast_types.FinishAsync(get_loc(), $5, $9, $13)} /**/
| BEGIN separated_list(SEMICOLON, expr) END { Ast_types.Seq(get_loc(), $2)} /**/
| simple_expr  expr  {Ast_types.App(get_loc(), $1, $2)} /**/


constructor_args:
| ID COLON simple_expr {Ast_types.ConstructorArgs($1,$3)}

