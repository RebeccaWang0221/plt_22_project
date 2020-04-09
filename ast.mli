type op = Add | Sub | Mult | Div | Mod | Eq | Neq | Less | And | Or | Lt | Gt | Lte | Gte | AddEq | SubEq | MultEq | DivEq

type un = Inc | Dec | Exp

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
  | Unop of expr * un  (* TODO: Unary operators *)
  | Call of string * expr list
  | Print of expr
  | Access of string * expr
  | Slice of string * expr * expr

(* statements *)
type stmt = 
    Block of stmt list
  | Expr of expr
  | If of expr * stmt list  (* TODO: if statements *)
  | While of expr * stmt list
  | For of expr * expr * expr * stmt list (* TODO: for in range *)
  | Do of stmt list * expr
  | Return of expr 
  | Assign of expr * expr
  | DecAssign of (typ * string) * expr
  | Cont 
  | Break
  | Pass

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
















