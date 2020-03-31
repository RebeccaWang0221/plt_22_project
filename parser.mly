(* Ocamlyacc parser for Rattlesnake *)

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS DIVID TIMES ASSIGN
%token EQ NEQ LT GT LTE GTE AND OR
%token IF ELSE ELIF FOR WHILE DO INT CHAR FLOAT STRING BOOL NULL
%token LIST DICT DEF
%token RETURN BREAK CONT COMMA
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BLIT
%token <string> ID
%token EOF

