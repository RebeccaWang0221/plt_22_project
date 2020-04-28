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

let rec string_of_typ = function
    Int -> "int"
  | Char -> "char"
  | String -> "string"
  | Float -> "float"
  | Stct -> "struct"
  | Bool -> "bool"
  | List(ty) -> "list<" ^ string_of_typ ty ^ ">"
  | Array(ty, sz) -> "array<" ^ string_of_typ ty ^ ">"











