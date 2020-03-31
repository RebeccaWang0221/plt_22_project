(* AST for Rattlesnake *)

type op = Add | Sub | Mult | Div | Mod | Eq | Neq | Less | And | Or | Lt | Gt | Lte | Gte

type typ = Int | String | Bool | Float | Char | Lst | Dict | Stct

type expr = 
    IntLit of int
  | StrLit of string
  | BoolLit of bool 
  | FloatLit of float 
  | CharLit of char 
  | Id of string 
  | Binop of expr * op * expr 
  | Assign of string * expr
  | Call of string * expr list
  | LstLit of expr list
  | DictLit (* TODO: of .... *)
  | StctLit (* TODO: of .... *)

type stmt = 
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For (* TODO: of .... *)
  | Return of expr
