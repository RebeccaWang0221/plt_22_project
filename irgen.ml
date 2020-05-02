module L = Llvm
module A = Ast
open Sast

(*
    *** Main concerns ***
      - How to implement list, array, and struct???
      - What is a phi node and how do we use it in for loops like Kaleidescope example???
*)

let translate stmts =
  (* create the LLVM compilation module to which we will generate the code *)
  let context = L.global_context () in
  let the_module = L.create_module context "RattleSnake" in
  let builder = L.builder context
  and local_vars = Hashtbl.create 30 in

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

  let main_ft = L.function_type i1_t [||] in
  let main_function = L.define_function "main" main_ft the_module
  (* this will act as a main function "wrapper" of sorts so that we can append blocks to it - trying to treat entire script as main function *)

  in

  let print_func =
    let ft = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    L.declare_function "print_func" ft the_module
  in

  let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
			| None -> ignore (instr builder) in

  in

  let lookup s =
    try Hashtbl.find local_vars s with (* check if s is a local variable *)
      | Not_found ->
        match L.lookup_global s the_module with (* check if s is a global variable *)
          | Some g -> g
          | None -> raise (Failure ("undeclared variable"))

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
        else let args1 = Array.map (build_expr builder) args in
        L.build_call callee args1 "call_func" builder
      | SAccess(id, e) -> (* TODO *)
      | SSlice(id, e1, e1) -> (* TODO *)

  in

  let rec build_stmt builder the_function = function
    | SExpr(e) -> ignore(build_expr builder e); builder
    | SBind(ty, id) -> ignore(L.declare_global (ltype_of_typ ty) id the_module); builder (* add variable to global scope aka the_module *)
  	| SFuncDef(func_def) -> (* TEST: no clue if this is right, tried to implement similar to microc *)
      Hashtbl.clear local_vars;
      let name = func_def.sfname in
      let params_arr = Array.of_list func_def.sformals in
      let params = Array.map (fun x ->
        match x with
          | SBind(ty, id) -> ltype_of_typ ty
          | _ -> raise (Failure ("invalid function argument"))
      ) params_arr in
      let ft = L.function_type (ltype_of_typ func_def.srtyp) params in (* define function type of return type and parameters *)
      let f = (* declare the function in the module *)
        match L.lookup_function name the_module with
          | None -> L.declare_function name ft the_module
          | Some f -> raise (Failure ("function " ^ name ^ " is already defined"))
      in
      let add_formal (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore(L.build_store p local builder);
        Hashtbl.add n local local_vars;
      and add_local (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder in
        Hashtbl.add n local_var local_vars;
      in
      List.iter2 add_formal func_def.sformals (Array.to_list (L.params f));
      List.iter add_local func_def.slocals;
      let bb = L.append_block context "func_body" f in (* create entry point block for function *)
      L.position_at_end bb builder;
      let rec build_body b = function (* recursively build each stmt in the body *)
        | [] -> []
        | _ as st :: tail ->
          let b' = build_stmt b f st in
          build_body b' tail
      in
      ignore(build_body builder func_def.sbody);
      L.position_at_end bb builder;             (* position builder at the end of the function block *)
      ignore(L.build_ret_void builder);         (* build a void return when function reaches end *)
      L.builder_at_end context bb
  	| SIf(e, body, dstmts) -> (* TEST *)
      let entry = L.append_block context "if_entry" the_function in (* create entry point *)
      let cond = build_expr (L.builder_at_end entry) e in
      let then_bb = L.append_block context "if_then" the_function in (* build block if conditional is true *)
      let rec build_body b = function (* recursively generate code for body *)
        | [] -> []
        | _ as st :: tail ->
          let b' = build_stmt b the_function st in
          build_body b' tail
      in
      ignore(build_body (L.builder_at_end context then_bb) body)); (* generate code starting at end of then_bb *)
      let end_bb = L.append_block context "if_end" the_function in
      let build_br_end = L.build_br end_bb in (* builds a branch to end_bb *)
      let rec build_dstmts b f = function
        | [] -> []
        | SElif(e, body) :: tail ->
          let elif_entry = L.append_block context "elif_entry" f in (* create an elif entry point bb *)
          let cond1 = build_expr (L.builder_at_end elif_entry) e in
          let elif_bb = L.append_block context "elif_then" f in (* create bb for elif body *)
          let res = build_dstmts b f tail in (* recursively build rest of dstmts from the bottom up *)
          if List.length res <> 0 then (* if there is dstmts following current elif, build conditional branch to other dstmts *)
            let next_bb = List.hd res in
            let b' = L.builder_at_end context elif_entry in
            ignore(L.build_cond_br cond1 elif_bb next_bb (L.builder_at_end elif_entry));
          else ignore(L.build_cond_br cond1 elif_bb end_bb (L.builder_at_end elif_entry)); (* otherwise build conditional branch to end_bb *)
          ignore(build_body (L.builder_at_end context elif_bb) body); (* build elif body *)
          ignore(L.build_br end_bb (L.builder_at_end context elif_bb)); [elif_entry] (* build branch to end_bb inside elif body, then return entrypoint *)
        | SElse(body) :: tail -> (* there is always only one else at the very end *)
          let bb = L.append_block context "else" f in (* create bb for else body *)
          ignore(build_body (L.builder_at_end bb) body);
          ignore(L.build_br end_bb (L.builder_at_end bb)); [bb] (* build branch to end_bb inside else body, then return bb *)
        | _ -> raise (Failure ("invalid dangling statement in if"))
      in
      let else_bb_lst = build_dstmts builder the_function dstmts in
      if List.length else_bb_lst > 0 then (* if there are dstmts, build a conditional branch to the other dstmts *)
        let else_bb = List.hd else_bb_lst in
        ignore(L.build_cond_br cond then_bb else_bb (L.builder_at_end entry));
      else ignore(L.build_cond_br cond then_bb end_bb (L.builder_at_end entry)); (* otherwise build conditional branch to end_bb *)
      ignore(L.build_br end_bb (L.builder_at_end then_bb)); (* build branch to end_bb at end of then_bb *)
      L.builder_at_end context end_bb
  	| SWhile(e, body) -> (* TEST *)
      let entry_bb = L.append_block context "while_entry" the_function in (* build entry block *)
      let cond = build_expr (L.builder_at_end entry_bb) e in (* build conditional inside of entry block *)
      let while_body = L.append_block context "while_body" the_function in
      let build_body b = function
        | [] -> []
        | _ as st :: tail ->
          let b' = build_stmt b the_function st in
          build_body b' tail
      in
      ignore(build_body (L.builder_at_end context while_body) body); (* build body inside of while_body block *)
      let end_bb = L.append_block context "while_end" the_function in
      ignore(L.build_br entry_bb (L.builder_at_end while_body)); (* branch to entry_bb at end of while_bb *)
      ignore(L.build_cond_br cond while_body end_bb (L.builder_at_end entry_bb)); (* conditional branch to while_body or end_body at the end of entry_bb *)
      L.builder_at_end context end_bb
  	| SFor(var, e, body) -> (* TODO: no clue how to do this yet *)
      builder
  	| SRange(var, e, body) -> (* TEST *)
      let start_val = L.const_int i32_t 0 in (* create start val at 0 *)
      let iterator = L.build_alloca i32_t "iter" builder in (* allocate stack space for iterator var *)
      ignore(L.build_store start_val iterator builder); (* store initial value for iterator *)
      let entry_bb = L.append_block context the_function in (* entry point *)
      let body_bb = L.append_block context the_function in
      let build_body b = function
        | [] -> []
        | _ as st :: tail ->
          let b' = build_stmt b the_function st in
          build_body b' body
      in
      ignore(build_body (L.builder_at_end context body_bb) body); (* build body inside of body_bb *)
      let body_builder = L.builder_at_end context body_bb in
      let val = L.build_load iterator "load_iter" body_builder in (* load iterator value from stack space *)
      let next_val = L.build_add val (L.const_int i32_t 1) "next_val" body_builder in (* increment iterator value by 1 *)
      ignore(L.build_store next_val iterator body_builder); (* store incremented iterator value on stack *)
      ignore(L.build_br entry_bb body_builder); (* branch back to entry_bb *)
      let end_bb = L.append_block context the_function in
      let end_val = build_expr builder e in
      let entry_builder = L.builder_at_end context entry_bb in
      let curr_val = L.build_load iterator "load_iter" entry_builder in (* in entry_bb, load value for iterator on stack *)
      let cond = L.build_icmp L.Icmp.Eq curr_val end_val "for_cmp" entry_builder in (* then check if it equals end_val *)
      ignore(L.build_cond_br cond end_bb body_bb entry_builder); (* conditional branch at end of entry_bb *)
      L.builder_at_end context end_bb
  	| SDo(body, e) ->
      let do_bb = L.append_block context the_function in (* create main loop body block *)
      ignore(L.build_br do_bb builder); (* force it to execute at least once *)
      let build_body b = function
        | [] -> []
        | _ as st :: tail ->
          let b' = build_stmt b the_function tail in
          build_body b' tail
      ignore(build_body (L.builder_at_end do_bb) body);
      let while_bb = L.append_block context the_function in
      let end_bb = L.append_block context the_function in
      let while_builder = L.builder_at_end context while_bb in
      let cond = build_expr while_builder e in
      ignore(L.build_cond_br cond do_bb end_bb while_builder); (* conditional branch at end of while_bb to either do_bb or end_bb *)
      ignore(L.build_br while_bb (L.builder_at_end context do_bb)); (* branch at end of do_bb to while_bb *)
      L.builder_at_end context end_bb
  	| SReturn(e) -> ignore(L.build_ret (build_expr builder e) builder); builder
  	| SAssign(s, e) ->
      let e' = build_expr builder e in
      ignore(L.build_store e' (lookup s) builder); builder (* build store function to load value into register *)
  	| SDecAssign(s, e) ->
      let (ty, id) = match s with
        | SBind(t, e) -> (t, e)
        | _ -> raise (Failure ("invalid declaration"))
      in
      ignore(L.declare_global (ltype_of_typ ty) id the_module); (* add variable to global scope aka the_module *)
      let e' = build_expr builder e in
      ignore(L.build_store e' (lookup id) builder); builder (* build store function to load value into register *)
  	| SStruct(id, body) -> (* TODO: no clue how to do this yet *)
      builder
    | SPrint(e) ->
      ignore(L.build_call print_func [| int_format_str ; (build_expr builder e) |] "print" builder); builder
  	| SCont -> ignore(L.build_br (L.block_of_value !continue_block) builder); builder
  	| SBreak -> ignore(L.build_br (L.block_of_value !break_block) builder); builder
  	| SPass -> (* TODO *)
      builder

  in

  List.iter (build_stmt builder main_function) stmts;
  the_module
