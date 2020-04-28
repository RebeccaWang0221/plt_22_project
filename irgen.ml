module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate stmts =
  (* create the LLVM compilation module to which we will generate the code *)
  let context = L.global_context () in
  let the_module = L.create_module context "RattleSnake" in
  let builder = L.builder context
  and var_map = Hashtbl.create 30 in

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

  let lookup s = Hashtbl.find var_map s (* TODO: need to check for local varables AND global variables *)

  in

  let rec build_expr builder ((t, e) : sexpr) =
    match e with
      | SIntLit i -> L.const_int i32_t i
      | SStrLit s -> L.build_global_stringptr s "string" builder
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit i -> L.const_float float_t i
      | SCharLit c -> L.const_int i8_t (Char.code c)
      | SId s -> L.build_load (lookup s) s builder
      | SBinop(e1, op, e2) -> (* TODO: going to need to handle case when adding different types, e.g. 2 + 5.5 *)
      	  match t with		    (* LLVM will throw an error when types do not match *)
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
       		  ) e1' e2' "float_binop" builder
      	    | A.String ->
      	      let e1' = build_expr builder e1
        	    and e2' = build_expr builder e2 in
        	  (match op with
          		| A.Add -> raise (Failure ("string concatenation not yet implemented"))
          		| A.Eq -> L.build_icmp L.Icmp.Eq
          		| A.Neq -> L.build_icmp L.Icmp.Ne
          		| _ -> raise (Failure ("invalid operation on type string"))
        	  ) e1' e1' "string_binop" builder
        	| A.Bool ->
        	  let e1' = build_expr builder e1
        	  and e2' = build_expr builder e2 in
        	  (match op with
        	    | A.Eq -> L.build_icmp L.Icmp.Eq
        	    | A.Neq -> L.build_icmp L.Icmp.Ne
        	    | A.And -> L.build_and
        	    | A.Or -> L.build_or
        	  ) e1' e2' "bool_binop" builder
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
       		  ) e1' e2' "int_binop" builder
      | SUnop(id, unop) ->
        let e' = build_expr builder e in
        (match unop with
          | A.Not -> L.build_not
          | _ -> raise (Failure ("invalid unary operator"))
        ) e' "unop" builder
      | SCall(name, args) ->
        let callee =
          match L.lookup_function name the_module with
            | Some c -> c
            | None -> raise (Failure ("unknown function referenced"))
        in
        let params = L.params callee
        and args_arr = Array.of_list args in
        if Array.length params <> Array.length args_arr then
          raise (Failure ("incorrect # of arguments passed"))
        else let args1 = Array.map build_expr args in (* not sure if this map call will work bcz build_expr needs builder as argument *)
        build_call callee args1 "call_func" builder
      | SAccess(id, e) -> (* TODO *)
      | SSlice(id, e1, e1) -> (* TODO *)

  in

  let rec build_stmt builder = function
    | SExpr(e) -> ignore(build_expr builder e); builder
    | SBind(ty, id) -> ignore(Hashtbl.add var_map id (ltype_of_typ ty)); builder (* add name: llvalue to var_map *)
  	| SFuncDef(func_def) ->
      let name = func_def.sfname in
      let params_arr = Array.of_list func_def.sformals in
      let params = Array.map (fun x ->
        match x with
          | SBind(ty, id) -> ltype_of_typ ty
          | _ -> raise (Failure ("invalid function argument"))
      ) params_arr in
      let ft = L.function_type func_def.srtyp params in (* define function type of return type and parameters *)
      let f = (* declare the function in the module *)
        match L.lookup_function name the_module with
          | None -> L.declare_function name ft the_module
          | Some f -> raise (Failure ("function " ^ name ^ " is already defined"))
      in
      Array.iteri (fun i a -> (* add parameters to var_map *)
        let n = snd param_arr.(i) in
        L.set_value_name n a;
        Hashtbl.add var_map n a;
      ) (L.params f);
      let bb = L.append_block context "entry" f in (* create entry point block for function *)
      L.position_at_end bb builder;
        (*
          TODO: need to recursively generate code for the each stmt in the body, and use build_ret or build_ret_void for
          return stmts, then call Llvm_analysis.assert_valid_function to check for bugs, finally return the created function
        *)
  	| SIf(e, body, dstmts) ->
      let cond = build_expr builder e in
      let start_bb = L.insertion_block builder
      let then_bb = L.insert_block context "then" start_bb in
        (* TODO: finish rest of SIf - copied from Kaleidoscope tutorial but cannot figure out *)
  	| SElif(e, body) -> (* TODO *)
  	| SElse(body) -> (* TODO *)
  	| SWhile(e, body) -> (* TODO *)
  	| SFor(var, e, body) -> (* TODO *)
  	| SRange(var, e, body) -> (* TODO *)
  	| SDo(body, e) -> (* TODO *)
  	| SReturn(e) -> ignore(L.build_ret (build_expr builder e) builder); builder
  	| SAssign(s, e) ->
      let e' = build_expr builder e in
      ignore(L.build_store e' (lookup s) builder); builder (* TODO: define lookup function *)
  	| SDecAssign(s, e) -> (* TODO *)
  	| SStruct(id, body) -> (* TODO *)
    | SPrint(e) -> (* TODO *)
  	| SCont -> (* TODO *)
  	| SBreak -> (* TODO *)
  	| SPass -> (* TODO *)

  in

  List.iter build_stmt stmts;
  the_module
