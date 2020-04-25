open Ast

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | And -> "and"
  | Or -> "or"

let string_of_typ = function
    Int -> "int"
  | Char -> "char"
  | String -> "string"
  | Float -> "float"
  | Lst -> "list"
  | Stct -> "struct"
  | Bool -> "bool"











