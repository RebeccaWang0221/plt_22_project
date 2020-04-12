%{ open Ast %}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK PLUS MINUS DIVIDE TIMES MOD PEQ MEQ TEQ DEQ ASSIGN
%token EQ NEQ LT GT LTE GTE AND OR NOT INC DEC EXP
%token IF ELSE ELIF FOR WHILE DO IN INT CHAR FLOAT STRING BOOL NONE
%token LIST STCT DEF RANGE
%token RETURN BREAK CONT PASS COMMA PRINT
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
    stmt_list EOF  { List.rev $1 }

stmt_list:
    { [] } 
  | stmt stmt_list  { $1::$2 }

stmt: 
    expr SEMI  { Expr $1 }
  | vdecl SEMI  { $1 }
  | fdecl  { $1 }
  | LBRACE stmt_list RBRACE  { Block $2 }
  | IF expr LBRACE stmt_list RBRACE dstmt  { If($2, $4, List.rev $6) }
  | WHILE expr LBRACE stmt_list RBRACE  { While($2, $4) }
  | FOR vdecl IN RANGE LPAREN expr RPAREN LBRACE stmt_list RBRACE  { For($2, $6, $9) }
  | FOR vdecl IN expr LBRACE stmt_list RBRACE  { For($2, $4, $6) }
  | DO LBRACE stmt_list RBRACE WHILE expr SEMI { Do($3, $6) }
  | expr PEQ expr SEMI  { Assign($1, Binop($1, Add, $3)) }
  | expr MEQ expr SEMI  { Assign($1, Binop($1, Sub, $3)) }
  | expr TEQ expr SEMI  { Assign($1, Binop($1, Mult, $3)) }
  | expr DEQ expr SEMI  { Assign($1, Binop($1, Div, $3)) }
  | expr ASSIGN expr SEMI  { Assign($1, $3) }
  | vdecl ASSIGN expr SEMI  { DecAssign($1, $3) }
  | RETURN expr SEMI  { Return $2 }
  | CONT SEMI  { Cont }
  | BREAK SEMI  { Break }
  | PASS SEMI  { Pass }

dstmt:
    { [] }
  | ELIF expr LBRACE stmt_list RBRACE dstmt  { Elif($2, $4)::$6 } 
  | ELSE LBRACE stmt_list RBRACE dstmt { Else($3)::$5 }

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
  | expr EQ expr  { Binop($1, Eq, $3) }
  | expr NEQ expr  { Binop($1, Neq, $3) }
  | expr LT expr  { Binop($1, Lt, $3) }
  | expr GT expr  { Binop($1, Gt, $3) }
  | expr LTE expr  { Binop($1, Lte, $3) }
  | expr GTE expr  { Binop($1, Gte, $3) }
  | expr AND expr  { Binop($1, And, $3) }
  | expr OR expr  { Binop($1, Or, $3) }
  | ID INC  { Unop($1, Inc) }
  | ID DEC  { Unop($1, Dec) }
  | NOT ID  { Unop($2, Not) }
  | LPAREN expr RPAREN  { $2 }
  | ID LPAREN args_opt RPAREN  { Call($1, $3) }
  | PRINT LPAREN expr RPAREN  { Print($3) }
  | ID LBRACK expr RBRACK  { Access($1, $3) }
  | ID LBRACK expr COLON expr RBRACK  { Slice($1, $3, $5) }

typ:
    INT  { Int }
  | FLOAT  { Float }
  | STRING  { String }
  | BOOL  { Bool }
  | CHAR  { Char }
  | STCT  { Stct }
  | LIST  { Lst }

vdecl:
    typ ID  { Bind($1, $2) }

fdecl:
    DEF ID LPAREN fcall_args RPAREN LBRACE stmt_list RBRACE  { FuncDef($2, $4, $7) }

fcall_args:
    fargs_list  { $1 }

fargs_list:
    vdecl  { $1::[] }
  | vdecl COMMA fargs_list  { $1::$3 }

args_opt:
    { [] }     
  | args  { $1 }

args: 
    expr  { $1::[] }
  | expr COMMA args  { $1::$3 }
























