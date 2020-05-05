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
  | Void -> "void"
  | List(ty) -> "list<" ^ string_of_typ ty ^ ">"
  | Array(ty, sz) -> "array<" ^ string_of_typ ty ^ ">"

let strip_str st =
  let length = String.length st in
  String.sub st 1 (length - 2)

let strip_char ch = String.sub ch 1 1
