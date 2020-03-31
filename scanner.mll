(* Ocamllex scanner for Rattlesnake *)

(*{ open Parser }*)

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse 
    [' ' '\t' '\r' '\n']  { token lexbuf }
  | "#!"			{ comment lexbuf }
  | "("			{ LPAREN }
  | ")"			{ RPAREN }
  | "{"			{ LBRACE }
  | "}"			{ RBRACE }
  | "["     { RBRACK }
  | "]"     { LBRACK }
  | ";"			{ SEMI }
  | "+"			{ PLUS }
  | "-"     { MINUS }
  | "/"     { DIVIDE }
  | "*"     { TIMES }
  | "+="    { PEQ }
  | "-="    { MEQ }
  | "*="    { TEQ }
  | "/="    { DEQ }
  | "="			{ ASSIGN }
  | "=="		{ EQ }
  | "!="		{ NEQ }
  | ">"			{ GT }
  | "<"			{ LT }
  | ">="		{ GTE }
  | "<="		{ LTE }
  | "and"		{ AND }
  | "or"		{ OR }
  | "if"		{ IF }
  | "else"  { ELSE }
  | "elif"	{ ELIF }
  | "for"		{ FOR }
  | "while"	{ WHILE }
  | "do"		{ DO }
  | "return" 	{ RETURN }
  | "break"		{ BREAK }
  | "continue"	{ CONT }
  | "int"		{ INT }
  | "char"		{ CHAR }
  | "float"		{ FLOAT }
  | "string"	{ STRING }
  | "bool"		{ BOOL }
  | "true"		{ BLIT(true) }
  | "false"		{ BLIT(false) }
  | "list"		{ LIST }
  | "dict"		{ DICT }
  | "def"		{ DEF }
  | digit+ as lem { INTLIT(int_of_string lem) }
  | digit*'.'digit+ as lem { FLOATLIT(float_of_string lem) }
  | '"'letter*'"' as lem { STRLIT(lem) }
  | letter (digit | letter | '_')* as lem { ID(lem) }
  | eof 		{ EOF }
  | _ as char 	{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
    "!#"  { token lexbuf }
  | _     { comment lexbuf } 
















