type op = Add | Sub | Mult | Div | Mod | Eq | Neq | And | Or | Lt | Gt | Lte | Gte 

type un = Inc | Dec | Not

(* expressions *)
type expr = 
    IntLit of int
  | StrLit of string
  | BoolLit of bool 
  | FloatLit of float 
  | CharLit of char 
  | LstLit of expr list 
  | ArrayLit of expr list
  | Id of string 
  | Binop of expr * op * expr 
  | Unop of string * un
  | Call of string * expr list
  | Print of expr
  | Access of string * expr
  | Slice of string * expr * expr

type typ = Int | String | Bool | Float | Char | Lst | Stct | Void | Array of typ * expr

(* statements *)
type stmt = 
  | Expr of expr
  | Bind of typ * string
  | FuncDef of stmt * stmt list * stmt list
  | If of expr * stmt list * stmt list
  | Elif of expr * stmt list
  | Else of stmt list
  | While of expr * stmt list
  | For of stmt * expr * stmt list
  | Range of stmt * expr * stmt list
  | Do of stmt list * expr
  | Return of expr 
  | Assign of expr * expr
  | DecAssign of stmt * expr
  | DecArr of stmt * expr list
  | Struct of string * stmt list
  | Cont 
  | Break
  | Pass

type program = stmt list














