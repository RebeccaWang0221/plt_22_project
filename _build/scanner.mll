{ open Parser
  open Pretty
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
    [' ' '\t' '\r' '\n']  { token lexbuf }
  | "#!"			{ comment lexbuf }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "{"  { LBRACE }
  | "}"	  { RBRACE }
  | "["  { LBRACK }
  | "]"   { RBRACK }
  | ";"  { SEMI }
  | ":"  { COLON }
  | ","  { COMMA }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "/"  { DIVIDE }
  | "*"  { TIMES }
  | "**"  { EXP }
  | "++"  { INC }
  | "--"  { DEC }
  | "%"  { MOD }
  | "+="  { PEQ }
  | "-="  { MEQ }
  | "*="  { TEQ }
  | "/="  { DEQ }
  | "="  { ASSIGN }
  | "=="  { EQ }
  | "!="  { NEQ }
  | ">"  { GT }
  | "<"  { LT }
  | ">="  { GTE }
  | "<="  { LTE }
  | "and"  { AND }
  | "or"  { OR }
  | ("not" | "!")  { NOT }
  | "."  { DOT }
  | "if"  { IF }
  | "else"  { ELSE }
  | "elif"  { ELIF }
  | "in"  { IN }
  | "for"  { FOR }
  | "while"  { WHILE }
  | "do"  { DO }
  | "range"  { RANGE }
  | "return"  { RETURN }
  | "break"  { BREAK }
  | "continue"  { CONT }
  | "pass"  { PASS }
  | "int"  { INT }
  | "char"  { CHAR }
  | "float"  { FLOAT }
  | "string"  { STRING }
  | "bool"  { BOOL }
  | "void"  { VOID }
  | "true"  { BLIT(true) }
  | "false"  { BLIT(false) }
  | "list"  { LIST }
  | "append"  { APPEND }
  | "remove"  { REMOVE }
  | "insert"  { INSERT }
  | "pop"  { POP }
  | "index"  { INDEX }
  | "struct"  { STCT }
  | "def"  { DEF }
  | "print"  { PRINT }
  | "array"  { ARRAY }
  | "list"  { LIST }
  | digit+ as lem  { INTLIT(int_of_string lem) }
  | digit*'.'digit+ as lem  { FLOATLIT(float_of_string lem) }
  | '"'[^'"''\\']*('\\'_[^'"''\\']*)*'"' as lem  { STRLIT(strip_str lem) }
  | "'"letter"'" as lem  { CHARLIT(strip_char lem) }
  | letter (digit | letter | '_')* as lem  { ID(lem) }
  | eof  { EOF }
  | _ as char  { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    "!#"  { token lexbuf }
  | _  { comment lexbuf }
