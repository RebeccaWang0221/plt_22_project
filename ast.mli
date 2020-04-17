type op = Add | Sub | Mult | Div | Mod | Eq | Neq | Less | And | Or | Lt | Gt | Lte | Gte | AddEq | SubEq | MultEq | DivEq

type un = Inc | Dec | Not

type typ = Int | String | Bool | Float | Char | Lst | Stct 

(* expressions *)
type expr = 
    IntLit of int
  | StrLit of string
  | BoolLit of bool 
  | FloatLit of float 
  | CharLit of char 
  | LstLit of string (* TODO: change LstLit to hold list value not string *)
  | Id of string 
  | Binop of expr * op * expr 
  | Unop of string * un
  | Call of string * expr list
  | Print of expr
  | Access of string * expr
  | Slice of string * expr * expr

(* statements *)
type stmt = 
    Block of stmt list
  | Expr of expr
  | Bind of typ * string
  | FuncDef of (typ * string) * stmt list * stmt list
  | If of expr * stmt list * stmt list
  | Elif of expr * stmt list
  | Else of stmt list
  | While of expr * stmt list
  | For of stmt * expr * stmt list
  | Do of stmt list * expr
  | Return of expr 
  | Assign of expr * expr
  | DecAssign of stmt * expr
  | Struct of string * stmt list
  | Cont 
  | Break
  | Pass

type program = stmt list














