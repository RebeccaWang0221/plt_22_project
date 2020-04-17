open Ast
open Sast

module StringMap = Map.Make(String)

let check(stmts) =
	let add_to_map map data = 
		(* TODO: add variables to map *)

	let rec check_expr = function 
	  | IntLit l -> (Int, SIntLit l)
	  | StrLit l -> (String, SStrLit l)
	  | BoolLit l -> (Bool, SBoolLit l)
	  | FloatLit l -> (Float, SFloatLit l)
	  | CharLit l -> (Char, SCharLit l)
	  | LstLit l -> (Lst, SLstLit l) (* TODO: have to fix this *)
	  | Id 
	  | Binop 
	  | Unop 			
	  | Call 		(* TODO: check all expression types *)
	  | Print 
	  | Access 
	  | Slice 
	in 
	let check_stmts (decls: stmt list) =
		(* TODO: check all stmts *)
		(* have to check each stmt, so we will have to check expr's as well *)

