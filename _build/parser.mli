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
  | VOID
  | ARRAY
  | LIST
  | STCT
  | DEF
  | RANGE
  | APPEND
  | REMOVE
  | INSERT
  | POP
  | INDEX
  | RETURN
  | BREAK
  | CONT
  | PASS
  | COMMA
  | PRINT
  | DOT
  | INTLIT of (int)
  | FLOATLIT of (float)
  | BLIT of (bool)
  | STRLIT of (string)
  | CHARLIT of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
