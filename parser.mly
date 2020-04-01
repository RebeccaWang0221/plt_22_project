(* Ocamlyacc parser for Rattlesnake *)

%{ open Ast %}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK PLUS MINUS DIVIDE TIMES PEQ MEQ TEQ DEQ ASSIGN
%token EQ NEQ LT GT LTE GTE AND OR
%token IF ELSE ELIF FOR WHILE DO INT CHAR FLOAT STRING BOOL NULL
%token LIST DICT STCT DEF
%token RETURN BREAK CONT COMMA PRINT
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BLIT
%token <string> STRLIT
%token <string> ID
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
%left DIVIDE TIMES
%right DEQ TEQ

%%

program: 
    decls EOF  { (* TODO *) }

decls:
	{ [] }     /* nothing */
    vdecl SEMI decls  { (* TODO *) }
    fdecl decls       { (* TODO *) }

vdecl:
    typ ID  { (* TODO *) }

fdecl:
    DEF ID LPAREN fcall_args RPAREN LBRACE vdecl_list stmt_list RBRACE   { (* TODO *) }

typ:
	INT     { (* TODO *) }
	FLOAT   { (* TODO *) }
	STRING  { (* TODO *) }
	BOOL    { (* TODO *) }
	CHAR    { (* TODO *) }
	STCT    { (* TODO *) }
	LIST    { (* TODO *) }
	DICT    { (* TODO *) }

fcall_args:
    fargs_list  { (* TODO *) }

fargs_list:
    vdecl  { (* TODO *) }
    vdecl COMMA fargs_list  { (* TODO *) }

vdecl_list:
	{ [] }     /* nothing */
    vdecl SEMI vdecl_list  { (* TODO *) }

stmt_list:
    { [] }     /* nothing */
    stmt stmt_list

expr:
    BLIT      { (* TODO *) }
    INTLIT    { (* TODO *) }
    FLOATLIT  { (* TODO *) }
    STRLIT    { (* TODO *) }
    ID        { (* TODO *) }
    expr PLUS expr    { (* TODO *) }
    expr MINUS expr   { (* TODO *) }
    expr TIMES expr   { (* TODO *) }
    expr DIVIDE expr  { (* TODO *) }
    expr PEQ expr  { (* TODO *) }
    expr MEQ expr  { (* TODO *) }
    expr TEQ expr  { (* TODO *) }
    expr DEQ expr  { (* TODO *) }
    expr EQ expr   { (* TODO *) }
    expr NEQ expr  { (* TODO *) }
    expr LT expr   { (* TODO *) }
    expr GT expr   { (* TODO *) }
    expr LTE expr  { (* TODO *) }
    expr GTE expr  { (* TODO *) }
    expr AND expr  { (* TODO *) }
    expr OR expr   { (* TODO *) }
    ID ASSIGN expr  { (* TODO *) }
    LPAREN expr RPAREN  { (* TODO *) }
    ID LPAREN args_opt RPAREN  { (* TODO *) }
    ID LBRACK lst_op RBRACK  { (* TODO *) }

stmt: 
    expr SEMI  { (* TODO *) }
    IF expr LBRACE stmt RBRACE  { (* TODO *) }
    WHILE expr LBRACE stmt RBRACE  { (* TODO *) }
    /* TODO... */

args_opt:
    { [] }     /* nothing */
    args

args: 
    expr  { (* TODO *) }
    expr COMMA args  { (* TODO *) }

lst_op:
    INTLIT  { (* TODO *) }
    ID  { (* TODO *) }
    expr PLUS expr    { (* TODO *) }
    expr MINUS expr   { (* TODO *) }
    expr TIMES expr   { (* TODO *) }
    expr DIVIDE expr  { (* TODO *) }

























