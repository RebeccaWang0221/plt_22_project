(* Ocamlyacc parser for Rattlesnake *)

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS DIVIDE TIMES PEQ MEQ TEQ DEQ ASSIGN
%token EQ NEQ LT GT LTE GTE AND OR
%token IF ELSE ELIF FOR WHILE DO INT CHAR FLOAT STRING BOOL NULL
%token LIST DICT DEF
%token RETURN BREAK CONT COMMA
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




