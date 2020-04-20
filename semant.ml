open Ast
open Sast

module StringMap = Map.Make(String)

(* Inital call to check will pass in empty maps - vars: map for variable, funcs: map for function definitions *)
let check stmts (vars, funcs) =

	let type_of_id id map =
		try StringMap.find id map 
		with Not_found -> raise (Failure ("undeclared identifier " ^ id))

	in

	let check_bool_expr ex = 
		let (t, e) = check_expr e in
		match t with 
		  | Bool -> (t, e)
		  | _ -> raise (Failure ("expected expression of type bool but got " ^ t))

	in 

	let rec check_expr = function 
	  | IntLit l -> (Int, SIntLit l) (* return type bindings for literals *)

	  | StrLit l -> (String, SStrLit l)

	  | BoolLit l -> (Bool, SBoolLit l)

	  | FloatLit l -> (Float, SFloatLit l)

	  | CharLit l -> (Char, SCharLit l)

	  | LstLit l -> (Lst, SLstLit l) (* TODO: have to fix the list literal - ex: [1,2,3] *)

	  | Id var -> (type_of_id var vars, SId var)

	  | Binop(ex1, op, ex2) -> (* check ex1 and ex2 recursively *)
	  	let (t1, e1) = check_expr ex1 
	  	and (t2, e2) = check_expr ex2 in
	  	let err = "illegal binary operator " ^ op in
	  	if t1 = t2 then
	  	  let t = match op with
	  	    | Add | Sub when t1 = Int -> Int
	  	    | Add | Sub when t1 = Float -> Float
	  	    | Mult when t1 = Int -> Int
	  	    | Mult when t1 = Float -> Float
	  	    | Div -> Float
	  	    | Mod -> Int
	  	    | Eq | Neq -> Bool
	  	    | Lt | Gt | Lte | Gte when t1 = Int -> Bool
	  	    | Lt | Gt | Lte | Gte when t1 = Float -> Bool
	  	    | And | Or when t1 = Bool -> Bool
	  	    | _ -> raise (Failure err)
	  	  in
	  	  (t, SBinop((t1, e1), op, (t2, e2)))
	  	  (* TODO: allow mixed types of int and float operations, ex: 5.5 * 2 -> Float *)
	  	else 
	  	  | Add | Sub | Mult | Div when ((t1 = Int && t2 = Float) || (t1 = Float && t2 = Int)) -> Float
	  	  | _ -> raise (Failure err)

	  | Unop(var, un) -> (* check to ensure var is and id *)
	  	let ty = type_of_id var vars in
	  	let t = match un with
	  	  | Inc | Dec when ty = Int -> Int
	  	  | Not when ty = Bool -> Bool
	  	  | _ -> raise (Failure "illegal unary operator on type " ^ ty)
	  	in
	  	(t, SUnop(var, un))

	  | Call(fname, args) -> (* make sure arguments match types in func_def *)

	  | Print ex -> (* ensure ex is valid for print *)
	    let (t1, e1) = check_expr e in
	    let t = match t1 with
	      | Int | Float | Bool | String | Char | Lst -> t1
	      | _ -> raise (Failure ("cannot print expression of type " ^ t))
	    in
	    (t, SPrint(t1, e1))

	  | Access(var, ex) -> (* ensure var is of list type and ex results in an int *)

	  | Slice(var, ex1, ex2) -> (* ensure var is of list type and ex1 and ex2 result in int *)

	  | _ -> (* defualt *)

	and rec check_stmt = function
	  | Block st_lst ->	
	  	(* 
	  		recursively check each stmt in st_list
	  		??? maybe should call check on st_lst with new vars map empty ???
	  	*)
	  | Expr ex -> check_expr ex 

	  | Bind(ty, st) ->
	  	(* 
			if st is in vars, if it is then raise error
			if not in vars, add to map
			return (ty, SBind(ty, st))
	  	*)
	  | FuncDef(bind, st1_lst, st2_lst) -> (* add func def to map *)

	  | If(ex, st1_lst, st2_lst) -> SIf(check_bool_expr ex, check_stmt_list st1_lst, check_stmt_list st2_lst)
	  	
	  | Elif(ex, st_lst) -> SElif(check_bool_expr ex, check_stmt_list st_lst)
	  	
	  | Else st_lst -> SElse(check_stmt_list st_lst)
	  	
	  | While(ex, st_lst) -> SWhile(check_bool_expr ex, check_stmt_list st_lst)
	  	
	  | For(st1, ex, st2_lst) -> (* probably have to change this in scanner/parser so that "for int x in range(10)" works *)

	  | Do(st_lst, ex) -> SDo(check_stmt_list st_lst, check_bool_expr ex)
	  	
	  | Return ex -> 
	  	(* 
	  		check ex is valid expr
	  		check ex matches func def 
	  		??? Need to match return with surrounding function ???
	  	*)
	  | Assign(ex1, ex2) -> 
	  	let (t1, e1) = check_expr ex1
	  	and (t2, e2) = check_expr ex2 in
	  	let err = "illegal assignment, expected expression of type " ^ t1 ^ " but got expression of type " ^ t2 
	  	in
	  	if t1 = t2 then (t1, SAssign((t1, e1), (t2, e2)))
	  	else raise (Failure err)

	  | DecAssign(st, ex) -> 
	    let (t1, e1) = check_stmt st 
	    and (t2, e2) = check_expr ex in
	    let err = "illegal assignment, expected expression of type " ^ t1 ^ " but got expression of type " ^ t2
		in
		if t1 = t2 then (t1, SDecAssign(e1, (t2, e2)))
		else raise (Failure err)

	  | Struct(s, st_lst) -> (* ??? not sure what to do here *)

	  | Cont -> SCont

	  | Break -> SBreak

	  | Pass -> SPass

	in let rec check_stmt_list = function 
	  | [] -> []
	  | s :: sl -> check_stmt s :: check_stmt_list sl
	in 
	check_stmt_list stmts
















