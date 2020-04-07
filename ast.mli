type op = Add | Sub | Mult | Div | Mod | Eq | Neq | Less | And | Or | Lt | Gt | Lte | Gte | AddEq | SubEq | MultEq | DivEq

type typ = Int | String | Bool | Float | Char | Lst | Stct 

(* expressions *)
type expr = 
    IntLit of int
  | StrLit of string
  | BoolLit of bool 
  | FloatLit of float 
  | CharLit of char 
  | LstLit of string
  | Id of string 
  | Binop of expr * op * expr 
  | Assign of string * expr
  | DecAssign of (typ * string) * expr
  | Call of string * expr list
  | Print of expr
  | Slice of string * expr

(* statements *)
type stmt = 
    Block of stmt list
  | Expr of expr
  | If of expr * stmt list
  | Elif of expr * stmt
  | Else of stmt
  | While of expr * stmt list
  | For of expr * expr * expr * stmt list
  | Do of stmt list * expr
  | Return of expr 

(* bind types to variable names *)
type bind = typ * string

(* functions definitions *)
type func_def = {
  fname: string;        (* function name *)
  formals: bind list;   (* formal params *)
  locals: bind list;    (* local vars *)
  body: stmt list;      (* body of function *)
}

type program = bind list * func_def list
















