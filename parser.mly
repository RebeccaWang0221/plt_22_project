%{ open Ast %}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK PLUS MINUS PEQ MEQ TEQ DEQ ASSIGN
%token DIVIDE TIMES MOD
%token EQ NEQ LT GT LTE GTE AND OR NOT INC DEC EXP
%token IF ELSE ELIF FOR WHILE DO IN INT CHAR FLOAT STRING BOOL VOID
%token ARRAY LIST STCT DEF RANGE IRANGE
%token APPEND REMOVE INSERT POP INDEX LEN
%token RETURN BREAK CONT PASS COMMA PRINT DOT
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BLIT
%token <string> STRLIT
%token <string> CHARLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%nonassoc LBRACK
%left OR
%left AND
%left EQ NEQ IN
%left LT GT LTE GTE
%left PLUS MINUS
%right PEQ MEQ
%left DIVIDE TIMES MOD EXP
%right DEQ TEQ
%left DOT

%%

program:
    stmt_list EOF  { $1 }

stmt_list:
    { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI  { Expr $1 }
  | vdecl SEMI  { $1 }
  | fdecl  { $1 }
  | array_decl SEMI  { $1 }
  | list_decl SEMI  { $1 }
  | list_funcs  { $1 }
  | typ ID LBRACK expr RBRACK SEMI  { Bind(Array($1, $4), $2) }
  | IF expr LBRACE stmt_list RBRACE dstmt  { If($2, $4, $6) }
  | WHILE expr LBRACE stmt_list RBRACE  { While($2, $4) }
  | FOR vdecl IN RANGE LPAREN expr RPAREN LBRACE stmt_list RBRACE  { Range($2, IntLit(0), $6, IntLit(1), $9) }
  | FOR vdecl IN RANGE LPAREN expr COMMA expr RPAREN LBRACE stmt_list RBRACE  { Range($2, $6, $8, IntLit(1), $11) }
  | FOR vdecl IN RANGE LPAREN expr COMMA expr COMMA expr RPAREN LBRACE stmt_list RBRACE  { Range($2, $6, $8, $10, $13) }
  | FOR vdecl IN IRANGE LPAREN expr RPAREN LBRACE stmt_list RBRACE  { IRange($2, $6, $9) }
  | FOR vdecl IN expr LBRACE stmt_list RBRACE  { For($2, $4, $6) }
  | DO LBRACE stmt_list RBRACE WHILE expr SEMI { Do($3, $6) }
  | expr PEQ expr SEMI  { Assign($1, Binop($1, Add, $3)) }
  | expr MEQ expr SEMI  { Assign($1, Binop($1, Sub, $3)) }
  | expr TEQ expr SEMI  { Assign($1, Binop($1, Mult, $3)) }
  | expr DEQ expr SEMI  { Assign($1, Binop($1, Div, $3)) }
  | expr INC SEMI  { Assign($1, Binop($1, Add, IntLit(1))) }
  | expr DEC SEMI  { Assign($1, Binop($1, Sub, IntLit(1))) }
  | expr ASSIGN expr SEMI  { Assign($1, $3) }
  | vdecl ASSIGN expr SEMI  { DecAssign($1, $3) }
  | RETURN expr SEMI  { Return $2 }
  | STCT ID LBRACE vdecl_list RBRACE  { Struct($2, $4) }
  | PRINT LPAREN expr RPAREN SEMI  { Print($3) }
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
  | MINUS INTLIT  { IntLit(-$2) }
  | FLOATLIT  { FloatLit($1) }
  | STRLIT  { StrLit($1) }
  | CHARLIT  { CharLit($1) }
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
  | expr EXP expr  { Binop($1, Exp, $3) }
  | expr AND expr  { Binop($1, And, $3) }
  | expr OR expr  { Binop($1, Or, $3) }
  | expr IN expr  { Binop($1, In, $3) }
  | NOT ID  { Unop($2, Not) }
  | LPAREN expr RPAREN  { $2 }
  | ID LPAREN args_opt RPAREN  { Call($1, $3) }
  | LEN LPAREN expr RPAREN  { Len($3) }
  | expr LBRACK expr RBRACK  { Access($1, $3) }
  | expr DOT INDEX LPAREN expr RPAREN  { Index($1, $5) }
  | expr DOT POP LPAREN expr RPAREN  { Pop($1, $5) }
  | LBRACK args RBRACK  { ListLit($2) }

typ:
    INT  { Int }
  | FLOAT  { Float }
  | STRING  { String }
  | BOOL  { Bool }
  | CHAR  { Char }
  | VOID  { Void }

vdecl:
    typ ID  { Bind($1, $2) }

vdecl_list:
    vdecl SEMI  { $1::[] }
  | vdecl SEMI vdecl_list  { $1::$3 }

fdecl:
    DEF vdecl LPAREN fcall_args RPAREN LBRACE stmt_list RBRACE  { FuncDef($2, $4, $7) }

fcall_args:
    { [] }
  | fargs_list  { $1 }

fargs_list:
    vdecl  { $1::[] }
  | vdecl COMMA fargs_list  { $1::$3 }

args_opt:
    { [] }
  | args  { $1 }

args:
    expr  { $1::[] }
  | expr COMMA args  { $1::$3 }

array_decl:
    ARRAY LT typ GT ID LBRACK expr RBRACK  { Bind(Array($3, $7), $5) }

list_decl:
    LIST LT typ GT ID  { Bind(List($3), $5) }
  | LIST LT typ GT ID ASSIGN expr  { DecAssign(Bind(List($3), $5), $7) }

list_funcs:
    expr DOT APPEND LPAREN expr RPAREN SEMI  { Append($1, $5) }
  | expr DOT REMOVE LPAREN expr RPAREN SEMI  { Remove($1, $5) }
  | expr DOT INSERT LPAREN expr COMMA expr RPAREN SEMI  { Insert($1, $5, $7) }
