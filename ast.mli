(* AST for Rattlesnake *)

type op = Add | Sub | Mult | Div | Mod | Eq | Neq | Less | And | Or | Lt | Gt | Lte | Gte | AddEq | SubEq | MultEq | DivEq

type typ = Int | String | Bool | Float | Char | Lst of typ | Dict | Stct of string * ((string * ty) list)

(* expressions *)
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

(* statements *)
type stmt = 
    Block of stmt list
  | Expr of expr
  | If of expr * stmt
  | Elif of expr * stmt
  | Else stmt
  | While of expr * stmt
  | For expr * expr * expr * stmt
  | Return of expr

(* bind types to variable names *)t
type bind = typ * string

(* functions definitions *)
type func_def = {
  rtype: type;          (* return type *)
  fname: string;        (* function name *)
  formals: bind list;   (* formal params *)
  locals: bind list;    (* local vars *)
  body: stmt list;      (* body of function *)
}

type program = bind list * func_def list
















