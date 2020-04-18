open Ast
open Sast

module StringMap = Map.Make(String)

let check(stmts) =
	let add_to_map map data = 
		(* TODO: add variables to map *)

	let type_of_id id =
		(* TODO: check map for type of id *)

	let rec check_expr = function 
	  | IntLit l -> (Int, SIntLit l) (* return type bindings for literals *)
	  | StrLit l -> (String, SStrLit l)
	  | BoolLit l -> (Bool, SBoolLit l)
	  | FloatLit l -> (Float, SFloatLit l)
	  | CharLit l -> (Char, SCharLit l)
	  | LstLit l -> (Lst, SLstLit l) (* TODO: have to fix the list literal - ex: [1,2,3] *)
	  | Id var -> (type_of_id var, SId var)
	  | Binop(ex1, op, ex2) -> (* check ex1 and ex2 recursively *)
	  | Unop(var, un) -> (* check to ensure var is and id *)
	  | Call(fname, args) -> (* make sure arguments match types in func_def *)
	  | Print ex -> (* ensure ex is valid for print *)
	  | Access(var, ex) -> (* ensure var is of list type and ex results in an int *)
	  | Slice(var, ex1, ex2) -> (* ensure var is of list type and ex1 and ex2 result in int *)
	  | _ -> (* defualt *)
	and rec check_stmts (decls: stmt list) = function
	  | Block st_lst ->	(* recursively check each stmt in st_list *)
	  | Expr ex -> (* check_expr ex *)
	  | Bind(ty, st) -> (* ensure st does not already exist in map, then add to map *)
	  | FuncDef(bind, st1_lst, st2_lst) -> (* add func def to map *)
	  | If(ex, st1_lst, st2_lst) -> (* check ex is bool type, recursively check st1_lst and st2_lst *)
	  | Elif(ex, st_lst) -> (* check ex is bool type, recursively check st_lst *)
	  | Else st_lst -> (* recursively check st_lst *)
	  | While(ex, st_lst) -> (* check ex is bool type, recursively check st_lst *)
	  | For(st1, ex, st2_lst) -> (* probably have to change this so that "for int x in range(10)" works *)
	  | Do(st_lst, ex) -> (* recursively check st_lst and check ex is bool type *)
	  | Return ex -> (* check ex matches func def *)
	  | Assign(ex1, ex2) -> (* check ex1 type matches ex2 type, if not case is needed for gradual typing *)
	  | DecAssign(st, ex) -> (* ensure st type matches ex type *)
	  | Struct(s, st_lst) -> (* ??? not sure what to d *)
	  | Cont -> (* ??? not sure what to do *)
	  | Break -> (* ??? not sure what to d *)
	  | Pass -> (* ??? not sure what to d *)
	  | _ -> (* default *)
		(* TODO: check all stmts *)
		(* have to check each stmt, so we will have to check expr's as well *)

