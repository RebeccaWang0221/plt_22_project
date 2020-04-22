open Ast
open Sast

module StringMap = Map.Make(String)

let check stmts vars =

	let type_of_var map id =
		try StringMap.find id map
		with Not_found -> raise (Failure ("undeclared identifier " ^ id))
	in

	let add_var map name ty =
	  match name with 
	    | _ when StringMap.mem name map -> raise (Failure ("duplicate variable names"))
	    | _ -> StringMap.add name ty map

	let check_bool_expr ex = 
		let (t, e) = check_expr e in
		match t with 
		  | Bool -> (t, e)
		  | _ -> raise (Failure ("expected expression of type bool but got " ^ t))

	in 

	let rec check_func_locals var_map = function
	  | [] -> []
	  | Bind(t, s) as st :: tail -> 
	    let (_, (t1, e1)) = check_stmt var_map st in
	    (t1, e1) :: check_func_locals tail
	  | DecAssign(s, e) as st :: tail -> 
	    let (_, (t1, e1)) = check_stmt var_map st in
	    (t1, e1) :: check_func_locals tail
	  | _ :: tail -> check_func_locals tail

	in

	let rec check_func_body var_map = function
	  | [] -> []
	  | Bind(t, s) :: tail -> check_func_body tail
	  | Assign(e1, e1) :: tail -> check_func_body tail
	  | DecAssign(s, e) :: tail -> check_func_body tail
	  | _ as st :: tail -> 
	    let (_, (t1, e1)) = check_stmt var_map st in
	    (t1, e1) :: check_func_body tail

	in

	let rec check_expr var_map = function 
	  | IntLit l -> (var_map, (Int, SIntLit l)) (* return type bindings for literals *)

	  | StrLit l -> (var_map, (String, SStrLit l))

	  | BoolLit l -> (var_map, (Bool, SBoolLit l))

	  | FloatLit l -> (var_map, (Float, SFloatLit l))

	  | CharLit l -> (var_map, (Char, SCharLit l))

	  | LstLit l -> (var_map, (Lst, SLstLit l)) (* TODO: have to fix the list literal - ex: [1,2,3] *)

	  | Id var -> (var_map, (type_of_id var_map var, SId var))

	  | Binop(ex1, op, ex2) -> (* check ex1 and ex2 recursively *)
	  	let (_, (t1, e1)) = check_expr ex1 
	  	and (_, (t2, e2)) = check_expr ex2 in
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
	  	  (var_map, (t, SBinop((t1, e1), op, (t2, e2))))
	  	else 
	  	  let t = match op with 
	  	    | Add | Sub | Mult | Div when ((t1 = Int && t2 = Float) || (t1 = Float && t2 = Int)) -> Float
	  	    | _ -> raise (Failure err)
	  	  in
	  	  (var_map, (t, SBinop((t1, e1), op, (t2, e2))))

	  | Unop(var, un) -> (* check to ensure var is an id *)
	  	let ty = type_of_id var var_map in
	  	let t = match un with
	  	  | Inc | Dec when ty = Int -> Int
	  	  | Not when ty = Bool -> Bool
	  	  | _ -> raise (Failure "illegal unary operator on type " ^ ty)
	  	in
	  	(var_map (t, SUnop(var, un)))

	  | Call(fname, args) -> (* TODO: make sure arguments match types in func_def *)

	  | Print ex -> (* ensure ex is valid for print *)
	    let (_, (t1, e1)) = check_expr var_map e in
	    let t = match t1 with
	      | Int | Float | Bool | String | Char | Lst -> t1
	      | _ -> raise (Failure ("cannot print expression of type " ^ t))
	    in
	    (var_map, (t, SPrint(t1, e1)))

	  | Access(var, ex) -> (* TODO: ensure var is of list type and ex results in an int *)
	    let (_, (t1, e1)) = check_expr var_map var
	    and (_, (t2, e2)) = check_expr var_map ex in
	    if t1 = Lst && t2 = Int then (var_map, SAccess(var, (t2, e2)))
		else raise (Failure ("list access index must be of type int"))

	  | Slice(var, ex1, ex2) -> (* ensure var is of list type and ex1 and ex2 result in int *)
	    let (_, (t1, e1)) = check_expr var_map var
	    and (_, (t2, e2)) = check_expr var_map ex1
	    and (_, (t3, e3)) = check_expr var_map ex2 in
	    if t1 = Lst && t2 = Int and t3 = Int then (var_map, SSlice(var, (t2, e2), (t3, e3)))
		else raise (Failure ("list slice indices must be of type int"))

	and rec check_stmt var_map = function

	  | Expr ex -> check_expr var_map ex 

	  | Bind(ty, st) ->
	    let m = add_var var_map st ty in
	    (m, (ty, SBind(ty, st)))
	 
	  | FuncDef(vdec, st1_lst, st2_lst) -> (* TODO: add func def to map *)
	  	let Bind(ty, name) = vdec
	  	and params = check_stmt_list StringMap.empty st1_lst
	  	and locals = check_func_locals var_map st1_lst
	  	and body = check_func_body var_map st2_lst
	 	in
	 	(var_map, SFuncDef({ srtyp=ty; sfname=name; sformals=params; slocals=locals; sbody=body }))

	  | If(ex, st1_lst, st2_lst) -> 
	    (var_map, SIf(check_bool_expr ex, check_stmt_list var_map st1_lst, check_stmt_list var_map st2_lst))
	  	
	  | Elif(ex, st_lst) -> (var_map, SElif(check_bool_expr ex, check_stmt_list var_map st_lst))
	  	
	  | Else st_lst -> (var_map, SElse(check_stmt_list var_map st_lst))
	  	
	  | While(ex, st_lst) -> (var_map, SWhile(check_bool_expr ex, check_stmt_list var_map st_lst)
	  	
	  | For(st1, ex, st2_lst) -> (* TODO: have to check types match in cases like: for int x in [1,2,3] - for char c in "hello" *)

	  | Range(st1, ex, st2_lst) ->
	  	let (m1, (t1, e1)) = check_stmt var_map st1 
	  	and (m2, (t2, e2)) = check_expr m1 ex 
	    and sst_lst = check_stmt_list m st2_lst in
	  	if t1 = t2 then
	  	  match t1 with
	  	    | _ when (t1 = Int && t2 = Int) -> (m2, SRange((t1, e1), (t2, e2), sst_lst))
	  	    | _ -> raise (Failure ("for-range loop must be used with int types"))
	  	else raise Failure("for-range loop must be used with int types but given types do not match")

	  | Do(st_lst, ex) -> (var_map, SDo(check_stmt_list var_map st_lst, check_bool_expr ex))
	  	
	  | Return ex -> 
	  	(* TODO: 
	  		check ex is valid expr
	  		check ex matches func def 
	  		??? Need to match return with surrounding function ???
	  	*)
	  | Assign(ex1, ex2) -> 
	  	let (m1, (t1, e1)) = check_expr var_map ex1
	  	and (m2, (t2, e2)) = check_expr m1 ex2 in
	  	let err = "illegal assignment, expected expression of type " ^ t1 ^ " but got expression of type " ^ t2 
	  	in
	  	if t1 = t2 then (m2, (t1, SAssign((t1, e1), (t2, e2))))
	  	else raise (Failure err)

	  | DecAssign(st, ex) -> 
	    let (m1, (t1, e1)) = check_stmt var_map st 
	    and (m2, (t2, e2)) = check_expr m1 ex in
	    let err = "illegal assignment, expected expression of type " ^ t1 ^ " but got expression of type " ^ t2
		in
		if t1 = t2 then (m2, (t1, SDecAssign(e1, (t2, e2))))
		else raise (Failure err)

	  | Struct(s, st_lst) -> (* TODO: ??? not sure what to do here *)

	  | Cont -> (var_map, SCont)

	  | Break -> (var_map, SBreak)

	  | Pass -> (var_map, SPass)

	in let rec check_stmt_list var_map = function (* TODO: take in and pass around func map as well *)
	  | [] -> []
	  | s :: sl -> 
	    let (new_map, st) = check_stmt var_map s in
	    st :: check_stmt_list new_map sl
	in 
	check_stmt_list vars stmts

















