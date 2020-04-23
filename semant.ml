open Ast
open Sast
open Pretty

module StringMap = Map.Make(String)

let check stmts vars funcs =

	let type_of_var map id =
		try StringMap.find id map
		with Not_found -> raise (Failure ("undeclared identifier " ^ id))
	in

	let add_var map name ty =
	  match name with 
	    | _ when StringMap.mem name map -> raise (Failure ("duplicate variable names"))
	    | _ -> StringMap.add name ty map

	in

	let check_assign lvt rvt err = 
	  if lvt = rvt then lvt else raise (Failure (err))

	in

	let rec check_expr var_map func_map = function 
	  | IntLit l -> (var_map, func_map, (Int, SIntLit l)) (* return type bindings for literals *)

	  | StrLit l -> (var_map, func_map, (String, SStrLit l))

	  | BoolLit l -> (var_map, func_map, (Bool, SBoolLit l))

	  | FloatLit l -> (var_map, func_map, (Float, SFloatLit l))

	  | CharLit l -> (var_map, func_map, (Char, SCharLit l))

	  | LstLit l -> (var_map, func_map, (Lst, SLstLit l)) (* TODO: have to fix the list literal - ex: [1,2,3] *)

	  | Id var -> (var_map, func_map, (type_of_var var_map var, SId var))

	  | Binop(ex1, op, ex2) -> (* check ex1 and ex2 recursively *)
	  	let (_, _, (t1, e1)) = check_expr var_map func_map ex1 
	  	and (_, _, (t2, e2)) = check_expr var_map func_map ex2 in
	  	let err = "illegal binary operator " ^ string_of_op op in
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
	  	  (var_map, func_map, (t, SBinop((t1, e1), op, (t2, e2))))
	  	else 
	  	  let t = match op with 
	  	    | Add | Sub | Mult | Div when ((t1 = Int && t2 = Float) || (t1 = Float && t2 = Int)) -> Float
	  	    | _ -> raise (Failure err)
	  	  in
	  	  (var_map, func_map, (t, SBinop((t1, e1), op, (t2, e2))))

	  | Unop(var, un) -> (* check to ensure var is an id *)
	  	let ty = type_of_var var_map var in
	  	let t = match un with
	  	  | Inc | Dec when ty = Int -> Int
	  	  | Not when ty = Bool -> Bool
	  	  | _ -> raise (Failure ("illegal unary operator on type "  ^ string_of_typ ty))
	  	in
	  	(var_map, func_map, (t, SUnop(var, un)))

	  | Call(fname, args) -> (* make sure arguments match types in func_def *)
	  	let fd = StringMap.find fname func_map in
	  	let param_length = List.length fd.sformals in
	  	if List.length args != param_length then
	  	  raise (Failure ("expected " ^ string_of_int param_length ^ " arguments"))
	  	else let check_call (ft, _) e =
	  	  let (_, _, (t, e')) = check_expr var_map func_map e in
	  	  let err = "illegal argument found " ^ string_of_typ t ^ " expected " ^ string_of_typ ft in
	  	  (check_assign ft t err, e')
	  	in
	  	let args' = List.map2 check_call fd.sformals args in
	  	(var_map, func_map, (fd.srtyp, SCall(fname, args')))
	  	
	  | Print ex -> (* ensure ex is valid for print *)
	    let (_, _, (t1, e1)) = check_expr var_map func_map ex in
	    let t = match t1 with
	      | Int | Float | Bool | String | Char | Lst -> t1
	      | _ -> raise (Failure ("cannot print expression of type " ^ string_of_typ t1))
	    in
	    (var_map, func_map, (t, SPrint(t1, e1)))

	  | Access(var, ex) -> (* ensure var is of list or array type and ex results in an int *)
	    let t1 = type_of_var var_map var
	    and (_, _, (t2, e2)) = check_expr var_map func_map ex in
	    if t1 = Lst && t2 = Int then (var_map, func_map, (t1(* should return type of list elems, not Lst *), SAccess(var, (t2, e2))))
		else raise (Failure ("list/array access index must be of type int"))

	  | Slice(var, ex1, ex2) -> (* ensure var is of list or array type and ex1 and ex2 result in int *)
	    let t1 = type_of_var var_map var
	    and (_, _, (t2, e2)) = check_expr var_map func_map ex1
	    and (_, _, (t3, e3)) = check_expr var_map func_map ex2 in
	    if t1 = Lst && t2 = Int && t3 = Int then (var_map, func_map, (t1(* should be type Lst of typ *), SSlice(var, (t2, e2), (t3, e3))))
		else raise (Failure ("slice indices must be of type int"))

	in

	let check_bool_expr var_map func_map ex = 
		let (_, _, (t, e)) = check_expr var_map func_map ex in
		match t with 
		  | Bool -> (t, e)
		  | _ -> raise (Failure ("expected expression of type bool but got " ^ string_of_typ t))

	in
	(* 
		TODO: ocamlbuild is throwing 'unbound value check_stmt' error becuase check_stmt is defined after check_func_params,
			  check_func_locals, and check_func_body. Need to fix this (I think by using 'and' instead of 'in').
	*)
	let rec check_func_params var_map func_map = function
	  | [] -> ([], var_map)
	  | Bind(t, s) as st :: tail -> 
	    let (m, _, s) = check_stmt var_map func_map st in
	    let SBind(t1, e1) = s in
	    let ret = check_func_params m func_map tail in
	    ((t1, e1) :: fst ret, snd ret)
	  | _ -> raise (Failure ("illegal parameter in function definition"))
 
	in

	let rec check_func_locals var_map func_map = function
	  | [] -> ([], var_map)
	  | Bind(t, s) as st :: tail -> 
	    let (m, _, s) = check_stmt var_map func_map st in
	    let SBind(t1, e1) = s in
	    let ret = check_func_locals m func_map tail in
	    ((t1, e1) :: fst ret, snd ret)
	  | DecAssign(s, e) as st :: tail -> 
	    let (m, _, s) = check_stmt var_map func_map st in
	    let SDecAssign(bind, _) = s in
	    let SBind(t1, e1) = bind in
	    let ret = check_func_locals m func_map tail in 
	    ((t1, e1) :: fst ret, snd ret)
	  | _ :: tail -> check_func_locals var_map func_map tail


	in

	let rec check_func_body var_map func_map rtyp = function
	  | [] -> []
	  | Bind(t, s) :: tail -> check_func_body var_map func_map rtyp tail
	  | Return(ex) :: tail -> 
	    let (t1, e1) = check_expr var_map func_map ex in
	    if t1 = rtyp then
	      SReturn((t1, e1)) :: check_func_body var_map func_map rtyp tail
	    else raise (Failure ("the returned type does not match the function definition"))
	  | _ as st :: tail -> 
	    let (_, _, s) = check_stmt var_map func_map st in
	    s :: check_func_body var_map func_map rtyp tail

	in

	let rec check_stmt var_map func_map = function

	  | Expr ex -> let (_, _, (t, e)) = check_expr var_map func_map ex in (var_map, func_map, SExpr((t, e)))

	  | Bind(ty, st) ->
	    let m = add_var var_map st ty in
	    (m, func_map, SBind(ty, st))
	 
	  | FuncDef(vdec, formals, body) -> (* add func def to map *)
	  	let Bind(ty, name) = vdec
	  	and (params, m1) = check_func_params StringMap.empty func_map formals
	  	and (locals, m2) = check_func_locals m1 func_map body
	  	and body = check_func_body m2 func_map ty body
	 	in let fdef = { srtyp=ty; sfname=name; sformals=params; slocals=locals; sbody=body } in
	 	let func_map' = StringMap.add name fdef func_map in
	 	(var_map, func_map', SFuncDef(fdef))

	  | If(ex, st1_lst, st2_lst) -> 
	    (var_map, func_map, SIf(check_bool_expr var_map func_map ex, check_stmt_list var_map func_map st1_lst, check_stmt_list var_map func_map st2_lst))
	  	
	  | Elif(ex, st_lst) -> (var_map, func_map, SElif(check_bool_expr var_map func_map ex, check_stmt_list var_map func_map st_lst))
	  	
	  | Else st_lst -> (var_map, func_map, SElse(check_stmt_list var_map func_map st_lst))
	  	
	  | While(ex, st_lst) -> (var_map, func_map, SWhile(check_bool_expr var_map func_map ex, check_stmt_list var_map func_map st_lst))
	  	
	  | For(st1, ex, st2_lst) -> (* TODO: have to check types match in cases like: for int x in [1,2,3] - for char c in "hello" *)
	    (var_map, func_map, SFor())
	  | Range(st1, ex, st2_lst) ->
	  	let (m1, _, s1) = check_stmt var_map func_map st1 in
	  	let SBind(t1, e1) = s1 
	  	and (m2, _, (t2, e2)) = check_expr m1 func_map ex 
	    and sst_lst = check_stmt_list m func_map st2_lst in
	  	if t1 = t2 then
	  	  match t1 with
	  	    | _ when (t1 = Int && t2 = Int) -> (m2, func_map, SRange(s1, (t2, e2), sst_lst))
	  	    | _ -> raise (Failure ("for-range loop must be used with int types"))
	  	else raise Failure("for-range loop must be used with int types but given types do not match")

	  | Do(st_lst, ex) -> (var_map, func_map, SDo(check_stmt_list var_map func_map st_lst, check_bool_expr var_map func_map ex))
	  	
	  | Return ex -> (* if return is not inside of a function definition then raise error *)
	  	raise (Failure ("return must belong to a function definition"))
	  	
	  | Assign(ex1, ex2) -> 
	  	let (m1, _, (t1, e1)) = check_expr var_map func_map ex1
	  	and (m2, _, (t2, e2)) = check_expr m1 func_map ex2 in
	  	let err = "illegal assignment, expected expression of type " ^ string_of_typ t1 ^ " but got expression of type " ^ string_of_typ t2 
	  	in
	  	if t1 = t2 then (m2, func_map, SAssign((t1, e1), (t2, e2)))
	  	else raise (Failure err)

	  | DecAssign(st, ex) -> 
	    let (m1, _, s) = check_stmt var_map func_map st in
	    let SBind(t1, e1) = s 
	    and (m2, _, (t2, e2)) = check_expr m1 func_map ex in
	    let err = "illegal assignment, expected expression of type " ^ string_of_typ t1 ^ " but got expression of type " ^ string_of_typ t2
		in
		if t1 = t2 then (m2, func_map, SDecAssign(s, (t2, e2)))
		else raise (Failure err)

	  | Struct(s, st_lst) -> (* TODO: ??? not sure what to do here *)
	  	(var_map, func_map, SStruct())
	  | Cont -> (var_map, func_map, SCont)

	  | Break -> (var_map, func_map, SBreak)

	  | Pass -> (var_map, func_map, SPass)

	in

	let rec check_stmt_list var_map func_map = function 
	  | [] -> []
	  | s :: sl -> 
	    let (var_map', func_map', st) = check_stmt var_map func_map s in
	    st :: check_stmt_list var_map' func_map' sl
	in 
	check_stmt_list vars funcs stmts

















