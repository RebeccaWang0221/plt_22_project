type token =
  | SEMI
  | COLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | PLUS
  | MINUS
  | PEQ
  | MEQ
  | TEQ
  | DEQ
  | ASSIGN
  | DIVIDE
  | TIMES
  | MOD
  | EQ
  | NEQ
  | LT
  | GT
  | LTE
  | GTE
  | AND
  | OR
  | NOT
  | INC
  | DEC
  | EXP
  | IF
  | ELSE
  | ELIF
  | FOR
  | WHILE
  | DO
  | IN
  | INT
  | CHAR
  | FLOAT
  | STRING
  | BOOL
  | NONE
  | LIST
  | STCT
  | DEF
  | RANGE
  | RETURN
  | BREAK
  | CONT
  | PASS
  | COMMA
  | PRINT
  | INTLIT of (int)
  | FLOATLIT of (float)
  | BLIT of (bool)
  | STRLIT of (string)
  | ID of (string)
  | LSTLIT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
