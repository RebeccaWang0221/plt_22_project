type op = Add | Sub | Mult | Div | Mod | Eq | Neq | Less | And | Or | Lt | Gt | Lte | Gte | AddEq | SubEq | MultEq | DivEq

type un = Inc | Dec 

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
  | Bind of typ * string
  | FuncDef of string * stmt list * stmt list
  | If of expr * stmt list  (* TODO: if statements *)
  | While of expr * stmt list
  | ForRange of stmt * expr * stmt list (* TODO: for in object *)
  | Do of stmt list * expr
  | Return of expr 
  | Assign of expr * expr
  | DecAssign of stmt * expr
  | Cont 
  | Break
  | Pass

type program = stmt list
















