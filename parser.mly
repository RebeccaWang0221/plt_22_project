%{ open Ast %}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK PLUS MINUS DIVIDE TIMES MOD PEQ MEQ TEQ DEQ ASSIGN
%token EQ NEQ LT GT LTE GTE AND OR
%token IF ELSE ELIF FOR WHILE DO IN INT CHAR FLOAT STRING BOOL NONE
%token LIST STCT DEF
%token RETURN BREAK CONT COMMA PRINT
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BLIT
%token <string> STRLIT
%token <string> ID
%token <string> LSTLIT
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ 
%left LT GT LTE GTE 
%left PLUS MINUS
%right PEQ MEQ
%left DIVIDE TIMES MOD
%right DEQ TEQ

%%

program: 
    decls EOF  { $1 }

typ:
    INT  { Int }
  | FLOAT  { Float }
  | STRING  { String }
  | BOOL  { Bool }
  | CHAR  { Char }
  | STCT  { Stct }
  | LIST  { Lst }

decls:
	{ ([], []) }     /* nothing */
  | vdecl SEMI decls  { (($1::fst $3), snd $3) }
  | fdecl decls  { (fst $2, ($1::snd $2)) }

vdecl:
    typ ID  { ($1, $2) }
  | typ ID ASSIGN expr  { DecAssign(($1, $2), $4)}

vdecl_list:
    { [] }     /* nothing */
  | vdecl SEMI vdecl_list  { $1::$3 }

fdecl:
    DEF ID LPAREN fcall_args RPAREN LBRACE vdecl_list stmt_list RBRACE   
    { 
      {
        fname=$2;
        formals=$4;
        locals=$7;
        body=$8;
      }
    }

fcall_args:
    fargs_list  { $1 }

fargs_list:
    vdecl  { $1::[] }
  | vdecl COMMA fargs_list  { $1::$3 }

expr:
    BLIT  { BoolLit($1) }
  | INTLIT  { IntLit($1) }
  | FLOATLIT  { FloatLit($1) }
  | STRLIT  { StrLit($1) }
  | LSTLIT { LstLit($1) }
  | ID  { Id($1) }
  | expr PLUS expr  { Binop($1, Add, $3) }
  | expr MINUS expr  { Binop($1, Sub, $3) }
  | expr TIMES expr  { Binop($1, Mult, $3) }
  | expr DIVIDE expr  { Binop($1, Div, $3) }
  | expr MOD expr  { Binop($1, Mod, $3) }
  | expr PEQ expr  { Binop($1, AddEq, $3) }
  | expr MEQ expr  { Binop($1, SubEq, $3) }
  | expr TEQ expr  { Binop($1, MultEq, $3) }
  | expr DEQ expr  { Binop($1, DivEq, $3) }
  | expr EQ expr  { Binop($1, Eq, $3) }
  | expr NEQ expr  { Binop($1, Neq, $3) }
  | expr LT expr  { Binop($1, Lt, $3) }
  | expr GT expr  { Binop($1, Gt, $3) }
  | expr LTE expr  { Binop($1, Lte, $3) }
  | expr GTE expr  { Binop($1, Gte, $3) }
  | expr AND expr  { Binop($1, And, $3) }
  | expr OR expr  { Binop($1, Or, $3) }
  | ID ASSIGN expr  { Assign($1, $3) }
  | LPAREN expr RPAREN  { $2 }
  | ID LPAREN args_opt RPAREN  { Call($1, $3) }
  | PRINT LPAREN expr RPAREN  { Print($3) }
  | ID LBRACK expr RBRACK  { Slice($1, $3) }

stmt: 
    expr SEMI  { Expr $1 }
  | LBRACE stmt_list RBRACE  { Block $2 }
  | IF expr LBRACE stmt_list RBRACE if_stmts  { (If($2, $4), $6) }
  | WHILE expr LBRACE stmt_list RBRACE  { While($2, $4) }
  | FOR expr SEMI expr SEMI expr LBRACE stmt_list RBRACE  { For($2, $4, $6, $8) }
  | DO LBRACE stmt_list RBRACE WHILE expr SEMI { Do($3, $6) }

stmt_list:
    { [] }     /* nothing */
  | stmt stmt_list  { $1::$2 }
    
if_stmts:
    { [] }
  | ELIF expr LBRACE stmt_list RBRACE if_stmts  { (Elif($2, %4), $6) }
  | ELSE LBRACE stmt_list RBRACE  { Else($3) }

args_opt:
    { [] }     /* nothing */
  | args  { $1 }

args: 
    expr  { $1::[] }
  | expr COMMA args  { $1::$3 }
























