module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate stmts =
  (* create the LLVM compilation module to which we will generate the code *)
  let context = L.global_context () in
  let the_module = L.create_module context "RattleSnake" in

  (* get types from context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context 
  and float_t    = L.double_type context 
  and string_t   = L.pointer_type (L.i8_type context)
  and void_t     = L.void_type   context
  in (* TODO: add list, array, and struct *)

  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.String -> string_t
    | A.Char -> i8_t
    | A.Void -> void_t
  in

  let global_vars : L.llvalue StringMap.t =
    (* TODO *)
  in

  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    (* TODO *)
  in

  let rec build_expr builder ((t, e) : sexpr) =
    match e with
      | SIntLit i -> L.const_int i32_t i
      | SStrLit s -> L.build_global_stringptr s "string" builder
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit i -> L.const_float float_t i
      | SCharLit c ->
      | SId s -> L.build_load (lookup s) s builder
      | SBinop(e1, op, e2) -> 
      	  match t with
      	    | A.Float ->
      	      let e1' = build_expr builder e1
        	  and e2' = build_expr builder e2 in
        	  (match op with
          	    | A.Add -> L.build_fadd
          		| A.Sub -> L.build_fsub
          		| A.Div -> L.build_fdiv
          		| A.Mult-> L.build_fmul
          		| A.Mod -> L.build_frem
	            | A.Eq -> L.build_fcmp L.Fcmp.Oeq
	            | A.Neq -> L.build_fcmp L.Fcmp.One
	            | A.Lt -> L.build_fcmp L.Fcmp.Olt
	            | A.Gt -> L.build_fcmp L.Fcmp.Ogt
	            | A.Lte -> L.build_fcmp L.Fcmp.Ole
	            | A.Gte -> L.build_fcmp L.Fcmp.Oge
       		  ) e1' e2' "tmp" builder
      	    | A.String ->
      	      let e1' = build_expr builder e1
        	  and e2' = build_expr builder e2 in
        	  (match op with
          		| A.Add -> raise (Failure ("string concatenation not yet implemented"))
          		| A.Eq -> (* TODO *)
          		| A.Neq -> (* TODO *)
          		| _ -> raise (Failure ("invalid operation on type string"))
        	  ) (* TODO: return *)
        	| A.Bool -> (* TODO *)
        	| A.Int -> 
        	  let e1' = build_expr builder e1
        	  and e2' = build_expr builder e2 in
        	  (match op with
          	    | A.Add -> L.build_add
          		| A.Sub -> L.build_sub
          		| A.Div -> L.build_sdiv
          		| A.Mult-> L.build_mul
          		| A.Mod -> L.build_srem
	            | A.Eq -> L.build_icmp L.Icmp.Eq
	            | A.Neq -> L.build_icmp L.Icmp.Ne
	            | A.Lt -> L.build_icmp L.Icmp.Slt
	            | A.Gt -> L.build_icmp L.Icmp.Sgt
	            | A.Lte -> L.build_icmp L.Icmp.Sle
	            | A.Gte -> L.build_icmp L.Icmp.Sge
       		  ) e1' e2' "tmp" builder
      | SUnop(id, unop) -> (* TODO *)
      | SPrint(e) -> (* TODO *)
      | SCall(func, args) -> (* TODO *)
      | SAccess(id, e) -> (* TODO *)
      | SSlice(id, e1, e1) -> (* TODO *)
        
  in

  let build_func_body fdecl =
  	(* TODO *)

  in 

  let rec build_stmt st =
    match st with
      | SExpr(e) -> (* TODO *)
      | SBind(ty, id) -> (* TODO *)
  	  | SFuncDef(func_def) -> (* TODO *)
  	  | SIf(e, body, dstmts) -> (* TODO *)
  	  | SElif(e, body) -> (* TODO *)
  	  | SElse(body) -> (* TODO *)
  	  | SWhile(e, body) -> (* TODO *)
  	  | SFor(var, e, body) -> (* TODO *)
  	  | SRange(var, e, body) -> (* TODO *)
  	  | SDo(body, e) -> (* TODO *)
  	  | SReturn(e) -> (* TODO *)
  	  | SAssign(e1, e2) -> (* TODO *)
  	  | SDecAssign(s, e) -> (* TODO *)
  	  | SStruct(id, body) -> (* TODO *)
  	  | SCont -> (* TODO *)
  	  | SBreak -> (* TODO *)
  	  | SPass -> (* TODO *)

  in 

  List.iter build_stmt stmts;
  the_module


    





















