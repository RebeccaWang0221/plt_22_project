module L = Llvm
open Ast
open Sast
open Pretty

let translate stmts =
  (* create the LLVM compilation module to which we will generate the code *)
  let context = L.global_context () in
  let the_module = L.create_module context "RattleSnake" in
  (* let builder = L.builder context in *)
  let local_vars:(string, L.llvalue) Hashtbl.t = Hashtbl.create 30 in
  let global_vars:(string, L.llvalue) Hashtbl.t = Hashtbl.create 30 in

  (* get types from context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and string_t   = L.pointer_type (L.i8_type context)
  in

  let int_node_t = L.named_struct_type context "IntNode" in
  let _ = L.struct_set_body int_node_t [| i32_t ; L.pointer_type int_node_t |] false in
  let int_list_t = L.named_struct_type context "IntList" in
  let _ = L.struct_set_body int_list_t [| L.pointer_type int_node_t ; i32_t |] false in

  let float_node_t = L.named_struct_type context "FloatNode" in
  let _ = L.struct_set_body float_node_t [| float_t ; L.pointer_type float_node_t |] false in
  let float_list_t = L.named_struct_type context "FloatList" in
  let _ = L.struct_set_body float_list_t [| L.pointer_type float_node_t ; float_t |] false in

  let str_node_t = L.named_struct_type context "StrNode" in
  let _ = L.struct_set_body str_node_t [| string_t ; L.pointer_type str_node_t |] false in
  let str_list_t = L.named_struct_type context "StrList" in
  let _ = L.struct_set_body str_list_t [| L.pointer_type str_node_t ; i32_t |] false in

  let int_arr_t = L.named_struct_type context "IntArray" in
  let _ = L.struct_set_body int_arr_t [| L.pointer_type i32_t ; i32_t |] false in

  let float_arr_t = L.named_struct_type context "FloatArray" in
  let _ = L.struct_set_body float_arr_t [| L.pointer_type float_t ; i32_t |] false in

  let str_arr_t = L.named_struct_type context "StrArray" in
  let _ = L.struct_set_body str_arr_t [| L.pointer_type string_t ; i32_t |] false in

  let ltype_of_typ = function
    | Ast.Int -> i32_t
    | Ast.Bool -> i1_t
    | Ast.Float -> float_t
    | Ast.String -> string_t
    | Ast.Char -> string_t
    | Ast.Void -> i1_t
    | Ast.List(Ast.Int) | Ast.List(Ast.Bool) -> int_list_t
    | Ast.List(Ast.Float) -> float_list_t
    | Ast.List(Ast.String) | Ast.List(Ast.Char) -> str_list_t
    | Ast.Array(Ast.Int, _) | Ast.Array(Ast.Bool, _) -> int_arr_t
    | Ast.Array(Ast.Float, _) -> float_arr_t
    | Ast.Array(Ast.String, _) | Ast.Array(Ast.Char, _) -> str_arr_t
    | _ -> raise (Failure ("invalid type"))
  in

  let printf_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in

  let str_cmp_t : L.lltype = L.function_type i1_t [| string_t ; string_t |] in
  let str_cmp : L.llvalue = L.declare_function "str_comp" str_cmp_t the_module in
  let str_diff : L.llvalue = L.declare_function "str_diff" str_cmp_t the_module in
  let str_concat_t : L.lltype = L.function_type string_t [| string_t ; string_t |] in
  let str_concat : L.llvalue = L.declare_function "str_concat" str_concat_t the_module in
  let str_size_t : L.lltype = L.function_type i32_t [| string_t |] in
  let str_size : L.llvalue = L.declare_function "str_size" str_size_t the_module in
  let contains_strstr_t : L.lltype = L.function_type i1_t [| string_t ; string_t |] in
  let contains_strstr : L.llvalue = L.declare_function "contains_strstr" contains_strstr_t the_module in
  let access_str_t : L.lltype = L.function_type string_t [| string_t ; i32_t |] in
  let access_str : L.llvalue = L.declare_function "access_str" access_str_t the_module in

  let pow_int_t : L.lltype = L.function_type i32_t [| i32_t ; i32_t |] in
  let pow_int : L.llvalue = L.declare_function "pow_int" pow_int_t the_module in
  let pow_float_t : L.lltype = L.function_type float_t [| float_t ; float_t |] in
  let pow_float : L.llvalue = L.declare_function "pow_float" pow_float_t the_module in

  let init_int_list_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type int_list_t |] in
  let init_int_list : L.llvalue = L.declare_function "init_int_list" init_int_list_t the_module in
  let print_int_list : L.llvalue = L.declare_function "print_int_list" init_int_list_t the_module in
  let append_int_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type int_list_t ; i32_t |] in
  let append_int : L.llvalue = L.declare_function "append_int" append_int_t the_module in
  let remove_int : L.llvalue = L.declare_function "remove_int" append_int_t the_module in
  let get_int_t : L.lltype = L.function_type i32_t [| L.pointer_type int_list_t ; i32_t |] in
  let get_int : L.llvalue = L.declare_function "get_int" get_int_t the_module in
  let insert_int_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type int_list_t ; i32_t ; i32_t |] in
  let insert_int : L.llvalue = L.declare_function "insert_int" insert_int_t the_module in
  let pop_int : L.llvalue = L.declare_function "pop_int" get_int_t the_module in
  let index_of_int : L.llvalue = L.declare_function "index_int" get_int_t the_module in
  let int_list_size_t : L.lltype = L.function_type i32_t [| L.pointer_type int_list_t |] in
  let int_list_size : L.llvalue = L.declare_function "int_list_size" int_list_size_t the_module in
  let contains_int_t : L.lltype = L.function_type i1_t [| L.pointer_type int_list_t ; i32_t |] in
  let contains_int : L.llvalue = L.declare_function "contains_int" contains_int_t the_module in
  let assign_int : L.llvalue = L.declare_function "assign_int" insert_int_t the_module in
  let copy_int_list_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type int_list_t ; L.pointer_type int_list_t |] in
  let copy_int_list : L.llvalue = L.declare_function "copy_int_list" copy_int_list_t the_module in

  let init_float_list_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type float_list_t |] in
  let init_float_list : L.llvalue = L.declare_function "init_float_list" init_float_list_t the_module in
  let print_float_list : L.llvalue = L.declare_function "print_float_list" init_float_list_t the_module in
  let append_float_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type float_list_t ; float_t |] in
  let append_float : L.llvalue = L.declare_function "append_float" append_float_t the_module in
  let remove_float_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type float_list_t ; i32_t |] in
  let remove_float : L.llvalue = L.declare_function "remove_float" remove_float_t the_module in
  let get_float_t : L.lltype = L.function_type float_t [| L.pointer_type float_list_t ; i32_t |] in
  let get_float : L.llvalue = L.declare_function "get_float" get_float_t the_module in
  let insert_float_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type float_list_t ; i32_t ; float_t |] in
  let insert_float : L.llvalue = L.declare_function "insert_float" insert_float_t the_module in
  let pop_float : L.llvalue = L.declare_function "pop_float" get_float_t the_module in
  let index_of_float_t : L.lltype = L.function_type i32_t [| L.pointer_type float_list_t ; float_t |] in
  let index_of_float : L.llvalue = L.declare_function "index_float" index_of_float_t the_module in
  let float_list_size_t : L.lltype = L.function_type i32_t [| L.pointer_type float_list_t |] in
  let float_list_size : L.llvalue = L.declare_function "float_list_size" float_list_size_t the_module in
  let contains_float_t : L.lltype = L.function_type i1_t [| L.pointer_type float_list_t ; float_t |] in
  let contains_float : L.llvalue = L.declare_function "contains_float" contains_float_t the_module in
  let assign_float : L.llvalue = L.declare_function "assign_float" insert_float_t the_module in
  let copy_float_list_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type float_list_t ; L.pointer_type float_list_t |] in
  let copy_float_list : L.llvalue = L.declare_function "copy_float_list" copy_float_list_t the_module in

  let init_str_list_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type str_list_t |] in
  let init_str_list : L.llvalue = L.declare_function "init_str_list" init_str_list_t the_module in
  let print_str_list : L.llvalue = L.declare_function "print_str_list" init_str_list_t the_module in
  let append_str_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type str_list_t ; string_t |] in
  let append_str : L.llvalue = L.declare_function "append_str" append_str_t the_module in
  let remove_str_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type str_list_t ; i32_t |] in
  let remove_str : L.llvalue = L.declare_function "remove_str" remove_str_t the_module in
  let get_str_t : L.lltype = L.function_type string_t [| L.pointer_type str_list_t ; i32_t |] in
  let get_str : L.llvalue = L.declare_function "get_str" get_str_t the_module in
  let insert_str_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type str_list_t ; i32_t ; string_t |] in
  let insert_str : L.llvalue = L.declare_function "insert_str" insert_str_t the_module in
  let pop_str : L.llvalue = L.declare_function "pop_str" get_str_t the_module in
  let index_of_str_t : L.lltype = L.function_type i32_t [| L.pointer_type str_list_t ; string_t |] in
  let index_of_str : L.llvalue = L.declare_function "index_str" index_of_str_t the_module in
  let str_list_size_t : L.lltype = L.function_type i32_t [| L.pointer_type str_list_t |] in
  let str_list_size : L.llvalue = L.declare_function "str_list_size" str_list_size_t the_module in
  let contains_str_t : L.lltype = L.function_type i1_t [| L.pointer_type str_list_t ; string_t |] in
  let contains_str : L.llvalue = L.declare_function "contains_str" contains_str_t the_module in
  let assign_str : L.llvalue = L.declare_function "assign_str" insert_str_t the_module in
  let copy_str_list_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type str_list_t ; L.pointer_type str_list_t |] in
  let copy_str_list : L.llvalue = L.declare_function "copy_str_list" copy_str_list_t the_module in

  let init_int_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type int_arr_t ; i32_t |] in
  let init_int_arr : L.llvalue = L.declare_function "init_int_arr" init_int_arr_t the_module in
  let assign_int_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type int_arr_t ; i32_t ; i32_t |] in
  let assign_int_arr : L.llvalue = L.declare_function "assign_int_arr" assign_int_arr_t the_module in
  let get_int_arr_t : L.lltype = L.function_type i32_t [| L.pointer_type int_arr_t ; i32_t |] in
  let get_int_arr : L.llvalue = L.declare_function "get_int_arr" get_int_arr_t the_module in
  let int_arr_size_t : L.lltype = L.function_type i32_t [| L.pointer_type int_arr_t |] in
  let int_arr_size : L.llvalue = L.declare_function "int_arr_size" int_arr_size_t the_module in
  let contains_int_arr_t : L.lltype = L.function_type i1_t [| L.pointer_type int_arr_t ; i32_t |] in
  let contains_int_arr : L.llvalue = L.declare_function "contains_int_arr" contains_int_arr_t the_module in
  let print_int_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type int_arr_t |] in
  let print_int_arr : L.llvalue = L.declare_function "print_int_arr" print_int_arr_t the_module in
  let copy_int_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type int_arr_t ;  L.pointer_type int_arr_t |] in
  let copy_int_arr : L.llvalue = L.declare_function "copy_int_arr" copy_int_arr_t the_module in

  let init_float_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type float_arr_t ; i32_t |] in
  let init_float_arr : L.llvalue = L.declare_function "init_float_arr" init_float_arr_t the_module in
  let assign_float_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type float_arr_t ; i32_t ; float_t |] in
  let assign_float_arr : L.llvalue = L.declare_function "assign_float_arr" assign_float_arr_t the_module in
  let get_float_arr_t : L.lltype = L.function_type float_t [| L.pointer_type float_arr_t ; i32_t |] in
  let get_float_arr : L.llvalue = L.declare_function "get_float_arr" get_float_arr_t the_module in
  let float_arr_size_t : L.lltype = L.function_type i32_t [| L.pointer_type float_arr_t |] in
  let float_arr_size : L.llvalue = L.declare_function "float_arr_size" float_arr_size_t the_module in
  let contains_float_arr_t : L.lltype = L.function_type i1_t [| L.pointer_type float_arr_t ; float_t |] in
  let contains_float_arr : L.llvalue = L.declare_function "contains_float_arr" contains_float_arr_t the_module in
  let print_float_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type float_arr_t |] in
  let print_float_arr : L.llvalue = L.declare_function "print_float_arr" print_float_arr_t the_module in
  let copy_float_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type float_arr_t ;  L.pointer_type float_arr_t |] in
  let copy_float_arr : L.llvalue = L.declare_function "copy_float_arr" copy_float_arr_t the_module in

  let init_str_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type str_arr_t ; i32_t |] in
  let init_str_arr : L.llvalue = L.declare_function "init_str_arr" init_str_arr_t the_module in
  let assign_str_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type str_arr_t ; i32_t ; string_t |] in
  let assign_str_arr : L.llvalue = L.declare_function "assign_str_arr" assign_str_arr_t the_module in
  let get_str_arr_t : L.lltype = L.function_type string_t [| L.pointer_type str_arr_t ; i32_t |] in
  let get_str_arr : L.llvalue = L.declare_function "get_str_arr" get_str_arr_t the_module in
  let str_arr_size_t : L.lltype = L.function_type i32_t [| L.pointer_type str_arr_t |] in
  let str_arr_size : L.llvalue = L.declare_function "str_arr_size" str_arr_size_t the_module in
  let contains_str_arr_t : L.lltype = L.function_type i1_t [| L.pointer_type str_arr_t ; string_t |] in
  let contains_str_arr : L.llvalue = L.declare_function "contains_str_arr" contains_str_arr_t the_module in
  let print_str_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type str_arr_t |] in
  let print_str_arr : L.llvalue = L.declare_function "print_str_arr" print_str_arr_t the_module in
  let copy_str_arr_t : L.lltype = L.function_type (L.void_type context) [| L.pointer_type str_arr_t ;  L.pointer_type str_arr_t |] in
  let copy_str_arr : L.llvalue = L.declare_function "copy_str_arr" copy_str_arr_t the_module in

  (* this will act as a main function "wrapper" of sorts so that we can append blocks to it - trying to treat entire script as main function *)
  let main_ft = L.function_type i32_t [||] in
  let main_function = L.define_function "main" main_ft the_module in

  let builder = L.builder_at_end context (L.entry_block main_function) in

  let int_format_str = L.build_global_stringptr "%d\n" "int_fmt" builder in
  let str_format_str = L.build_global_stringptr "%s\n" "str_fmt" builder in
  let float_format_str = L.build_global_stringptr "%f\n" "str_fmt" builder in
  let char_format_str = L.build_global_stringptr "%s\n" "char_fmt" builder in
  let bool_format_str = L.build_global_stringptr "%d\n" "bool_fmt" builder in

  let lookup s =
    try Hashtbl.find local_vars s with (* check if s is a local variable *)
      | Not_found -> Hashtbl.find global_vars s

  in

  let rec build_expr builder ((t, e) : sexpr) =
    match e with
      | SIntLit i -> L.const_int i32_t i
      | SStrLit s -> L.build_global_stringptr s "string" builder
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit i -> L.const_float float_t i
      | SCharLit c -> L.build_global_stringptr c "char" builder
      | SId s -> L.build_load (lookup s) s builder
      | SBinop(e1, op, e2) ->
      	(match t with
      	  | Ast.Float ->
            let (t1, _) = e1
            and (t2, _) = e2 in
            (match t1 with
              | Int ->
                (match t2 with
                  | Int ->
                    let int_e1 = build_expr builder e1 in
                    let e1' = L.build_sitofp int_e1 float_t "int_to_float" builder
                    and int_e2 = build_expr builder e2 in
                    let e2' = L.build_sitofp int_e2 float_t "int_to_float" builder in
                    (match op with
                      | Ast.Add -> L.build_fadd
                      | Ast.Sub -> L.build_fsub
                      | Ast.Div -> L.build_fdiv
                      | Ast.Mult-> L.build_fmul
                      | _ -> raise (Failure ("invalid binary operation"))
                    ) e1' e2' "float_binop" builder
                  | Float ->
                    let int_e1 = build_expr builder e1 in
                    let e1' = L.build_sitofp int_e1 float_t "int_to_float" builder
                    and e2' = build_expr builder e2 in
                    if op = Exp then L.build_call pow_float [| e1' ; e2' |] "pow_f" builder else
                    (match op with
                      | Ast.Add -> L.build_fadd
                      | Ast.Sub -> L.build_fsub
                      | Ast.Div -> L.build_fdiv
                      | Ast.Mult-> L.build_fmul
                      | _ -> raise (Failure ("invalid binary operation"))
                    ) e1' e2' "float_binop" builder
                  | _ -> raise (Failure ("invalid binary operation")))
              | Float ->
                (match t2 with
                  | Int ->
                    let e1' = build_expr builder e1
                    and int_e2 = build_expr builder e2 in
                    let e2' = L.build_sitofp int_e2 float_t "int_to_float" builder in
                    if op = Exp then L.build_call pow_float [| e1' ; e2' |] "pow_f" builder else
                    (match op with
                      | Ast.Add -> L.build_fadd
                      | Ast.Sub -> L.build_fsub
                      | Ast.Div -> L.build_fdiv
                      | Ast.Mult-> L.build_fmul
                      | _ -> raise (Failure ("invalid binary operation"))
                    ) e1' e2' "float_binop" builder
                  | Float ->
                    let e1' = build_expr builder e1
                    and e2' = build_expr builder e2 in
                    if op = Exp then L.build_call pow_float [| e1' ; e2' |] "pow_f" builder else
                    (match op with
                      | Ast.Add -> L.build_fadd
                      | Ast.Sub -> L.build_fsub
                      | Ast.Div -> L.build_fdiv
                      | Ast.Mult-> L.build_fmul
                      | _ -> raise (Failure ("invalid binary operation"))
                    ) e1' e2' "float_binop" builder
                  | _ -> raise (Failure ("invalid binary operation")))
              | _ -> raise (Failure ("invalid binary operation")))
      	  | Ast.String ->
      	    let e1' = build_expr builder e1
        	  and e2' = build_expr builder e2 in
          	(match op with
              | Ast.Add -> L.build_call str_concat [| e1' ; e2' |] "str_cat" builder
              | _ -> raise (Failure ("invalid operation on type string"))
          	)
        	| Ast.Bool ->
            let (t1, _) = e1
            and (t2, _) = e2 in
            (match t1 with
              | Int ->
                if op = In then
                  let (ty, s) = (match e2 with
                    | (t1, SId(s1)) -> (t1, s1)
                    | _ -> raise (Failure ("invalid in operation")))
                  in
                  let pointer = lookup s in
                  let e1' = build_expr builder e1 in
                  (match ty with
                    | List(_) -> L.build_call contains_int [| pointer ; e1' |] "cont_list" builder
                    | Array(_, _) -> L.build_call contains_int_arr [| pointer ; e1' |] "cont_int" builder
                    | _ -> raise (Failure ("invalid in operation")))
                else
                (match t2 with
                  | Int ->
                    let e1' = build_expr builder e1
                    and e2' = build_expr builder e2 in
                    (match op with
                      | Ast.Eq -> L.build_icmp L.Icmp.Eq
                      | Ast.Neq -> L.build_icmp L.Icmp.Ne
                      | Ast.Gt -> L.build_icmp L.Icmp.Sgt
                      | Ast.Lt -> L.build_icmp L.Icmp.Slt
                      | Ast.Gte -> L.build_icmp L.Icmp.Sge
                      | Ast.Lte -> L.build_icmp L.Icmp.Sle
                      | _ -> raise (Failure ("invalid bool operation on type int"))
                    ) e1' e2' "bool_binop" builder
                  | Float ->
                    let int_e1 = build_expr builder e1 in
                    let e1' = L.build_sitofp int_e1 float_t "int_to_float" builder
                    and e2' = build_expr builder e2 in
                    (match op with
                      | Ast.Eq -> L.build_fcmp L.Fcmp.Oeq
                      | Ast.Neq -> L.build_fcmp L.Fcmp.One
                      | Ast.Gt -> L.build_fcmp L.Fcmp.Ogt
                      | Ast.Lt -> L.build_fcmp L.Fcmp.Olt
                      | Ast.Gte -> L.build_fcmp L.Fcmp.Oge
                      | Ast.Lte -> L.build_fcmp L.Fcmp.Ole
                      | _ -> raise (Failure ("invalid bool operation on type float"))
                    ) e1' e2' "bool_binop" builder
                  | _ -> raise (Failure ("invalid bool operation")))
              | Float ->
                if op = In then
                  let (ty, s) = (match e2 with
                    | (t1, SId(s1)) -> (t1, s1)
                    | _ -> raise (Failure ("invalid in operation")))
                  in
                  let pointer = lookup s in
                  let e1' = build_expr builder e1 in
                  (match ty with
                    | List(_) -> L.build_call contains_float [| pointer ; e1' |] "cont_f" builder
                    | Array(_, _) -> L.build_call contains_float_arr [| pointer ; e1' |] "cont_f" builder
                    | _ -> raise (Failure ("invalid float operation")))
                else
                (match t2 with
                  | Float ->
                    let e1' = build_expr builder e1
                    and e2' = build_expr builder e2 in
                    (match op with
                      | Ast.Eq -> L.build_fcmp L.Fcmp.Oeq
                      | Ast.Neq -> L.build_fcmp L.Fcmp.One
                      | Ast.Gt -> L.build_fcmp L.Fcmp.Ogt
                      | Ast.Lt -> L.build_fcmp L.Fcmp.Olt
                      | Ast.Gte -> L.build_fcmp L.Fcmp.Oge
                      | Ast.Lte -> L.build_fcmp L.Fcmp.Ole
                      | _ -> raise (Failure ("invalid bool operation on type float"))
                    ) e1' e2' "bool_binop" builder
                  | Int ->
                    let e1' = build_expr builder e1
                    and int_e2 = build_expr builder e2 in
                    let e2' = L.build_sitofp int_e2 float_t "int_to_float" builder in
                    (match op with
                      | Ast.Eq -> L.build_fcmp L.Fcmp.Oeq
                      | Ast.Neq -> L.build_fcmp L.Fcmp.One
                      | Ast.Gt -> L.build_fcmp L.Fcmp.Ogt
                      | Ast.Lt -> L.build_fcmp L.Fcmp.Olt
                      | Ast.Gte -> L.build_fcmp L.Fcmp.Oge
                      | Ast.Lte -> L.build_fcmp L.Fcmp.Ole
                      | _ -> raise (Failure ("invalid bool operation on type float"))
                    ) e1' e2' "bool_binop" builder
                  | _ -> raise (Failure ("invalid bool operation on type float")))
              | Char ->
                if op = In then
                  let (ty, s) = (match e2 with
                    | (t1, SId(s1)) -> (t1, s1)
                    | _ -> raise (Failure ("invalid in operation")))
                  in
                  (match ty with
                    | List(t1) ->
                      let pointer = lookup s in
                      let e1' = build_expr builder e1 in
                      L.build_call contains_str [| pointer ; e1' |] "cont_st" builder
                    | Array(_, _) ->
                      let pointer = lookup s in
                      let e1' = build_expr builder e1 in
                      L.build_call contains_str_arr [| pointer ; e1' |] "cont_st" builder
                    | _ ->
                      let e1' = build_expr builder e1
                      and e2' = build_expr builder e2 in
                      L.build_call contains_strstr [| e2' ; e1' |] "cont_st" builder)
                else
                let e1' = build_expr builder e1
                and e2' = build_expr builder e2 in
                (match op with
                  | Ast.Eq -> L.build_call str_cmp [| e1' ; e2' |] "eq_st" builder
                  | Ast.Neq -> L.build_call str_diff [| e1' ; e2' |] "neq_st" builder
                  | _ -> raise (Failure ("invalid comparison operation on chars")))
              | String ->
                if op = In then
                  let (ty, s) = (match e2 with
                    | (t1, SId(s1)) -> (t1, s1)
                    | _ -> raise (Failure ("invalid in operation")))
                  in
                  let pointer = lookup s in
                  let e1' = build_expr builder e1 in
                  (match ty with
                    | List(_) -> L.build_call contains_str [| pointer ; e1' |] "cont_st" builder
                    | Array(_, _) -> L.build_call contains_str_arr [| pointer ; e1' |] "cont_st" builder
                    | _ -> raise (Failure ("invalid in operation")))
                else
                let e1' = build_expr builder e1
                and e2' = build_expr builder e2 in
                (match op with
                  | Ast.Eq -> L.build_call str_cmp [| e1' ; e2' |] "eq_st" builder
                  | Ast.Neq -> L.build_call str_diff [| e1' ; e2' |] "neq_st" builder
                  | _ -> raise (Failure ("invalid comparison operation on strings")))
              | Bool ->
                if op = In then
                  let (ty, s) = match e2 with
                    | (t1, SId(s1)) -> (t1, s1)
                    | _ -> raise (Failure ("invalid call to in operation"))
                  in
                  let pointer = lookup s in
                  let e1' = build_expr builder e1 in
                  (match ty with
                    | List(_) -> L.build_call contains_int [| pointer ; (L.const_intcast e1' i32_t false) |] "cont_i" builder
                    | Array(_, _) -> L.build_call contains_int_arr [| pointer ; (L.const_intcast e1' i32_t false) |] "cont_i" builder
                    | _ -> raise (Failure ("invalid in operation")))
                else
                let e1' = build_expr builder e1
                and e2' = build_expr builder e2 in
                (match op with
                  | Ast.And -> L.build_and e1' e2' "and" builder
                  | Ast.Or -> L.build_or e1' e2' "or" builder
                  | _ -> raise (Failure ("invalid operation on bool types")))
              | _ -> raise (Failure ("invalid binary operation of type bool")))
        	| Ast.Int ->
        	  let e1' = build_expr builder e1
        	  and e2' = build_expr builder e2 in
            if op = Exp then L.build_call pow_int [| e1' ; e2' |] "pow_i" builder else
        	  (match op with
          	  | Ast.Add -> L.build_add
          		| Ast.Sub -> L.build_sub
          		| Ast.Div -> L.build_sdiv
          		| Ast.Mult-> L.build_mul
          		| Ast.Mod -> L.build_srem
              | _ -> raise (Failure ("invalid binary operation on type int"))
       		  ) e1' e2' "int_binop" builder
          | _ -> raise (Failure ("invalid binary operation")))
      | SUnop(id, unop) ->
        let var = lookup id in
        let var_val = L.build_load var "load" builder in
        if unop <> Not then
          raise (Failure ("invalide unary operation"))
        else L.build_not var_val "not" builder
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
        else let args1 = Array.map (build_expr builder) args_arr in
        L.build_call callee args1 "call_func" builder
      | SAccess(id, e) ->
        (match id with
          | (List(t), SId(s)) ->
            let pointer = lookup s in
            let idx = build_expr builder e in
            (match t with
              | Int | Bool -> L.build_call get_int [| pointer ; idx |] "access" builder
              | Float -> L.build_call get_float [| pointer ; idx |] "access" builder
              | String | Char -> L.build_call get_str [| pointer ; idx |] "access" builder
              | _ -> raise (Failure ("invalid type for list in access call")))
          | (Array(t, sz), SId(s)) ->
            let pointer = lookup s in
            let idx = build_expr builder e in
            (match t with
              | Int | Bool -> L.build_call get_int_arr [| pointer ; idx |] "acces" builder
              | Float -> L.build_call get_float_arr [| pointer ; idx |] "access" builder
              | String | Char -> L.build_call get_str_arr [| pointer ; idx |] "access" builder
              | _ -> raise (Failure ("invalid type for array in access call")))
          | (String, _) ->
            let s1 = build_expr builder id in
            let idx = build_expr builder e in
            L.build_call access_str [| s1 ; idx |] "access" builder
          | _ -> raise (Failure ("invalid access call")))
      | SIndex(id, e) ->
        let (t, s) = (match id with
          | (List(t1), SId(s1)) -> (t1, s1)
          | _ -> raise (Failure ("invalid call to index on non list type")))
        in
        let pointer = lookup s in
        let v = build_expr builder e in
        (match t with
          | Int -> L.build_call index_of_int [| pointer ; v |] "index" builder
          | Bool -> L.build_call index_of_int [| pointer ; (L.const_intcast v i32_t false) |] "index" builder
          | Float -> L.build_call index_of_float [| pointer ; v |] "index" builder
          | String | Char -> L.build_call index_of_str [| pointer ; v |] "index" builder
          | _ -> raise (Failure ("invalid type for list")))
      | SPop(id, e) ->
        let (t, s) = (match id with
          | (List(t1), SId(s1)) -> (t1, s1)
          | _ -> raise (Failure ("invalid pop call on on list type")))
        in
        let pointer = lookup s in
        let v = build_expr builder e in
        (match t with
          | Int | Bool -> L.build_call pop_int [| pointer; v |] "pop" builder
          | Float -> L.build_call pop_float [| pointer; v |] "pop" builder
          | String | Char -> L.build_call pop_str [| pointer; v |] "pop" builder
          | _ -> raise (Failure ("invalid type for list")))
      | SLen(e) ->
        let (t1, e1) = e in
        (match t1 with
          | String ->
            let v = build_expr builder e in
            L.build_call str_size [| v |] "len" builder
          | List(ty) ->
            let s = (match e1 with
              | SId(s1) -> s1
              | _ -> raise (Failure ("invalid call to len on list")))
            in
            let pointer = lookup s in
            (match ty with
              | Int | Bool -> L.build_call int_list_size [| pointer |] "len" builder
              | Float -> L.build_call float_list_size [| pointer |] "len" builder
              | String | Char -> L.build_call str_list_size [| pointer |] "len" builder
              | _ -> raise (Failure ("invalid type for list")))
          | Array(ty, sz) ->
            let s = (match e1 with
              | SId(s1) -> s1
              | _ -> raise (Failure ("invalid call to len on array")))
            in
            let pointer = lookup s in
            (match ty with
              | Int | Bool -> L.build_call int_arr_size [| pointer |] "len" builder
              | Float -> L.build_call float_arr_size [| pointer |] "len" builder
              | String | Char -> L.build_call str_arr_size [| pointer |] "len" builder
              | _ -> raise (Failure ("invalid type for array")))
          | _ -> raise (Failure ("invalid call to len with type " ^ string_of_typ t1)))
      | _ -> raise (Failure ("invalid expression, something is wrong"))

  in

  let rec build_list_lit p b = function
    | [] -> []
    | _ as ex :: tail ->
      let (t, _) = ex in
      let v1 = build_expr b ex in
      (match t with
        | Int ->
          ignore(L.build_call append_int [| p ; v1 |] "" b);
          build_list_lit p b tail
        | Bool ->
          ignore(L.build_call append_int [| p ; (L.const_intcast v1 i32_t false) |] "" b);
          build_list_lit p b tail
        | Float ->
          ignore(L.build_call append_float [| p ; v1 |] "" b);
          build_list_lit p b tail
        | String | Char ->
          ignore(L.build_call append_str [| p ; v1 |] "" b);
          build_list_lit p b tail
        | _ -> raise (Failure ("invalid type for list object")))

  in

  let rec build_arr_lit p b idx = function
    | [] -> []
    | _ as ex :: tail ->
      let (t, _) = ex in
      let v1 = build_expr b ex in
      (match t with
        | Int ->
          ignore(L.build_call assign_int_arr [| p ; (L.const_int i32_t idx) ; v1 |] "" b);
          build_arr_lit p b (idx + 1) tail
        | Bool ->
          ignore(L.build_call assign_int_arr [| p ; (L.const_int i32_t idx) ; (L.const_intcast v1 i32_t false) |] "" b);
          build_arr_lit p b (idx + 1) tail
        | Float ->
          ignore(L.build_call assign_float_arr [| p ; (L.const_int i32_t idx) ; v1 |] "" b);
          build_arr_lit p b (idx + 1) tail
        | String | Char ->
          ignore(L.build_call assign_str_arr [| p ; (L.const_int i32_t idx) ; v1 |] "" b);
          build_arr_lit p b (idx + 1) tail
        | _ -> raise (Failure ("invalid type for array object")))

  in

  let rec build_body b f var_map = function
    | [] -> b
    | _ as st :: tail ->
      let b' = build_stmt b f var_map st in
      build_body b' f var_map tail

  and build_dstmts b f end_bb var_map = function
    | [] -> []
    | SElif(e, body) :: tail ->
      let elif_entry = L.append_block context "elif_entry" f in (* create an elif entry point bb *)
      let cond1 = build_expr (L.builder_at_end context elif_entry) e in
      let elif_bb = L.append_block context "elif_then" f in (* create bb for elif body *)
      ignore(L.move_block_after elif_bb end_bb);
      let res = build_dstmts b f end_bb var_map tail in (* recursively build rest of dstmts from the bottom up *)
      if List.length res <> 0 then (* if there is dstmts following current elif, build conditional branch to other dstmts *)
        let next_bb = List.hd res in
        let _ = L.builder_at_end context elif_entry in
        ignore(L.build_cond_br cond1 elif_bb next_bb (L.builder_at_end context elif_entry));
      else ignore(L.build_cond_br cond1 elif_bb end_bb (L.builder_at_end context elif_entry)); (* otherwise build conditional branch to end_bb *)
      ignore(build_body (L.builder_at_end context elif_bb) f var_map body); (* build elif body *)
      ignore(L.build_br end_bb (L.builder_at_end context elif_bb)); (* build branch to end_bb inside elif body, then return entrypoint *)
      [elif_entry]
    | SElse(body) :: tail -> (* there is always only one else at the very end *)
      let bb = L.append_block context "else" f in (* create bb for else body *)
      ignore(build_body (L.builder_at_end context bb) f var_map body);
      ignore(L.build_br end_bb (L.builder_at_end context bb)); (* build branch to end_bb inside else body, then return bb *)
      ignore(L.move_block_after bb end_bb);
      [bb]
    | _ -> raise (Failure ("invalid dangling statement in if"))

    and build_stmt builder the_function var_map = function
    | SExpr(e) -> ignore(build_expr builder e); builder
    | SBind(ty, id) ->
      let pointer = L.build_alloca (ltype_of_typ ty) id builder in
      (match ty with
        | List(t) ->
          (match t with
            | Int | Bool ->
              ignore(L.build_call init_int_list [| pointer |] "" builder);
              Hashtbl.add var_map id pointer;
              builder
            | Float ->
              ignore(L.build_call init_float_list [| pointer |] "" builder);
              Hashtbl.add var_map id pointer;
              builder
            | String | Char ->
              ignore(L.build_call init_str_list [| pointer |] "" builder);
              Hashtbl.add var_map id pointer;
              builder
            | _ -> raise (Failure ("invalid type in list declaration")))
        | Array(t, sz) ->
          let s = (match sz with
            | IntLit(s1) -> s1
            | _ -> raise (Failure ("invalid array declaration")))
          in
          let size = L.const_int i32_t s in
          (match t with
            | Int | Bool ->
              ignore(L.build_call init_int_arr [| pointer ; size |] "" builder);
              Hashtbl.add var_map id pointer;
              builder
            | Float ->
              ignore(L.build_call init_float_arr [| pointer ; size |] "" builder);
              Hashtbl.add var_map id pointer;
              builder
            | String | Char ->
              ignore(L.build_call init_str_arr [| pointer ; size |] "" builder);
              Hashtbl.add var_map id pointer;
              builder
            | _ -> raise (Failure ("invalid type in array declaration")))
        | _ -> Hashtbl.add var_map id pointer;
          builder)
  	| SFuncDef(func_def) ->
      Hashtbl.clear local_vars;
      let name = func_def.sfname in
      let params_arr = Array.of_list func_def.sformals in
      let params = Array.map (fun x ->
        match x with
          | (ty, id) -> ltype_of_typ ty
      ) params_arr in
      let ft = L.function_type (ltype_of_typ func_def.srtyp) params in (* define function type of return type and parameters *)
      let f = (* declare the function in the module *)
        match L.lookup_function name the_module with
          | None -> L.define_function name ft the_module
          | Some f -> raise (Failure ("function " ^ name ^ " is already defined"))
      in
      let func_builder = L.builder_at_end context (L.entry_block f) in
      let add_formal (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n func_builder in
        ignore(L.build_store p local func_builder);
        Hashtbl.add local_vars n local;
      and add_local (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n func_builder in
        Hashtbl.add local_vars n local_var;
      in
      List.iter2 add_formal func_def.sformals (Array.to_list (L.params f));
      List.iter add_local func_def.slocals;
      let final_builder = build_body func_builder f local_vars func_def.sbody in
      (match func_def.srtyp with
        | Void | Bool -> ignore(L.build_ret (L.const_int i1_t 0) final_builder);
        | Int -> ignore(L.build_ret (L.const_int i32_t 0) final_builder);
        | Float -> ignore(L.build_ret (L.const_float float_t 0.0) final_builder);
        | String | Char -> ignore(L.build_ret (L.build_global_stringptr "" "char" final_builder) final_builder);
        | _ -> ignore(););
      Hashtbl.clear local_vars;
      builder
  	| SIf(e, body, dstmts) ->
      let entry = L.append_block context "if_entry" the_function in (* create entry point *)
      ignore (L.build_br entry builder);
      let cond = build_expr (L.builder_at_end context entry) e in
      let then_bb = L.append_block context "if_then" the_function in (* build block if conditional is true *)
      ignore(build_body (L.builder_at_end context then_bb) the_function var_map body); (* generate code starting at end of then_bb *)
      let end_bb = L.append_block context "if_end" the_function in
      let else_bb_lst = build_dstmts builder the_function end_bb var_map dstmts in
      if List.length else_bb_lst > 0 then (* if there are dstmts, build a conditional branch to the other dstmts *)
        let else_bb = List.hd else_bb_lst in
        ignore(L.build_cond_br cond then_bb else_bb (L.builder_at_end context entry));
      else ignore(L.build_cond_br cond then_bb end_bb (L.builder_at_end context entry)); (* otherwise build conditional branch to end_bb *)
      ignore(L.build_br end_bb (L.builder_at_end context then_bb)); (* build branch to end_bb at end of then_bb *)
      L.builder_at_end context end_bb
  	| SWhile(e, body) ->
      let entry_bb = L.append_block context "while_entry" the_function in (* build entry block *)
      ignore (L.build_br entry_bb builder);
      let cond = build_expr (L.builder_at_end context entry_bb) e in (* build conditional inside of entry block *)
      let while_body = L.append_block context "while_body" the_function in
      let body_builder = build_body (L.builder_at_end context while_body) the_function var_map body in (* build body inside of while_body block *)
      let end_bb = L.append_block context "while_end" the_function in
      ignore(L.build_br entry_bb body_builder); (* branch to entry_bb at end of while_bb *)
      ignore(L.build_cond_br cond while_body end_bb (L.builder_at_end context entry_bb)); (* conditional branch to while_body or end_body at the end of entry_bb *)
      L.builder_at_end context end_bb
  	| SFor(var, e, body) ->
      let (t, n) = (match var with
        | SBind(t1, n1) -> (t1, n1)
        | _ -> raise (Failure ("invalid for declaration")))
      in
      let (ty, s) = (match e with (* only works for ids *)
        | (tx, SId(sx)) -> (tx, sx)
        | _ -> raise (Failure ("for only works for ids")))
      in
      let start_iter_val = L.const_int i32_t 0 in (* index of list/string starts at 0 *)
      let iterator = L.build_alloca i32_t "iter" builder in (* allocate for the iterator *)
      let iter_value_p = (match t with (* allocate for the list/string iterator value *)
        | Int -> L.build_alloca i32_t "iter_val" builder
        | Float -> L.build_alloca float_t "iter_val" builder
        | String | Char -> L.build_alloca string_t "iter_val" builder
        | Bool -> L.build_alloca i1_t "iter_val" builder
        | _ -> raise (Failure ("invalid type on for loop iteration")))
      in
      Hashtbl.add local_vars "for_iter" iterator; (* add iterator  *)
      Hashtbl.add local_vars n iter_value_p; (* add iterator value *)
      ignore(L.build_store start_iter_val iterator builder); (* store iterator *)
      let iter_value = (match ty with
        | String ->
          let str_p = build_expr builder e in
          L.build_call access_str [| str_p ; start_iter_val |] "access" builder
        | List(_) ->
          let lst_pointer = lookup s in
          (match t with
            | Int | Bool -> L.build_call get_int [| lst_pointer ; start_iter_val |] "access" builder
            | Float -> L.build_call get_float [| lst_pointer ; start_iter_val |] "access" builder
            | String | Char -> L.build_call get_str [| lst_pointer ; start_iter_val |] "access" builder
            | _ -> raise (Failure ("invalid type on for loop iteration")))
        | Array(_, _) ->
          let arr_pointer = lookup s in
          (match t with
            | Int | Bool -> L.build_call get_int_arr [| arr_pointer ; start_iter_val |] "access" builder
            | Float -> L.build_call get_float_arr [| arr_pointer ; start_iter_val |] "access" builder
            | String | Char -> L.build_call get_str_arr [| arr_pointer ; start_iter_val |] "access" builder
            | _ -> raise (Failure ("invalid type on for loop iteration")))
        | _ -> raise (Failure ("invalid for declaration")))
      in
      ignore(L.build_store iter_value iter_value_p builder);
      let entry_bb = L.append_block context "for_entry" the_function in
      let end_val = (match ty with
        | String ->
          let str_p = build_expr builder e in
          L.build_call str_size [| str_p |] "end_val" builder
        | List(_) ->
          let lst_pointer = lookup s in
          (match t with
            | Int | Bool -> L.build_call int_list_size [| lst_pointer |] "end_val" builder
            | Float -> L.build_call float_list_size [| lst_pointer |] "end_val" builder
            | String | Char -> L.build_call str_list_size [| lst_pointer |] "end_val" builder
            | _ -> raise (Failure ("invalid type on for loop iteration")))
        | Array(_, _) ->
          let arr_pointer = lookup s in
          (match t with
            | Int | Bool -> L.build_call int_arr_size [| arr_pointer |] "end_val" builder
            | Float -> L.build_call float_arr_size [| arr_pointer |] "end_val" builder
            | String | Char -> L.build_call str_arr_size [| arr_pointer |] "end_val" builder
            | _ -> raise (Failure ("invalid type on for loop iteration")))
        | _ -> raise (Failure ("invalid for declaration")))
      in
      ignore(L.build_br entry_bb builder);
      let body_bb = L.append_block context "for_body" the_function in
      let body_builder = build_body (L.builder_at_end context body_bb) the_function var_map body in
      let step_bb = L.append_block context "for_step" the_function in
      ignore(L.build_br step_bb body_builder);
      let step_builder = L.builder_at_end context step_bb in
      let idx_val = L.build_load iterator "load_iter" step_builder in
      let next_val = L.build_add idx_val (L.const_int i32_t 1) "next_val" step_builder in
      ignore(L.build_store next_val iterator step_builder);
      let iter_value = (match ty with
        | String ->
          let str_p = build_expr step_builder e in
          L.build_call access_str [| str_p ; next_val |] "access" step_builder
        | List(_) ->
          let lst_pointer = lookup s in
          (match t with
            | Int | Bool -> L.build_call get_int [| lst_pointer ; next_val |] "access" step_builder
            | Float -> L.build_call get_float [| lst_pointer ; next_val |] "access" step_builder
            | String | Char -> L.build_call get_str [| lst_pointer ; next_val |] "access" step_builder
            | _ -> raise (Failure ("invalid type on for loop iteration")))
        | Array(_, _) ->
          let arr_pointer = lookup s in
          (match t with
            | Int | Bool -> L.build_call get_int_arr [| arr_pointer ; next_val |] "access" step_builder
            | Float -> L.build_call get_float_arr [| arr_pointer ; next_val |] "access" step_builder
            | String | Char -> L.build_call get_str_arr [| arr_pointer ; next_val |] "access" step_builder
            | _ -> raise (Failure ("invalid type on for loop iteration")))
        | _ -> raise (Failure ("invalid for declaration")))
      in
      ignore(L.build_store iter_value iter_value_p step_builder);
      ignore(L.build_br entry_bb step_builder);
      let end_bb = L.append_block context "for_end" the_function in
      let entry_builder = L.builder_at_end context entry_bb in
      let curr_val = L.build_load iterator "load_iter" entry_builder in
      let cond = L.build_icmp L.Icmp.Sge curr_val end_val "for_cmp" entry_builder in
      ignore(L.build_cond_br cond end_bb body_bb entry_builder);
      L.builder_at_end context end_bb
  	| SRange(var, beg, ed, st, body) ->
      let (t, n) = (match var with
        | SBind(t1, n1) -> (t1, n1)
        | _ -> raise (Failure ("invalid range declaration")))
      in
      let start_val = build_expr builder beg in (* create start val at 0 *)
      let iterator = L.build_alloca i32_t "iter" builder in (* allocate stack space for iterator var *)
      Hashtbl.add local_vars n iterator;
      ignore(L.build_store start_val iterator builder); (* store initial value for iterator *)
      let end_val = build_expr builder ed in
      let entry_bb = L.append_block context "range_entry" the_function in (* entry point *)
      ignore(L.build_br entry_bb builder);
      ignore(L.position_at_end entry_bb builder);
      let body_bb = L.append_block context "range_body" the_function in
      let body_builder = build_body (L.builder_at_end context body_bb) the_function var_map body in (* build body inside of body_bb *)
      let step_bb = L.append_block context "range_step" the_function in
      ignore(L.build_br step_bb body_builder);
      let step_builder = L.builder_at_end context step_bb in
      let tmp_val = L.build_load iterator "load_iter" step_builder in (* load iterator value from stack space *)
      let next_val = L.build_add tmp_val (build_expr step_builder st) "next_val" step_builder in (* increment iterator value by 1 *)
      ignore(L.build_store next_val iterator step_builder); (* store incremented iterator value on stack *)
      ignore(L.build_br entry_bb step_builder); (* branch back to entry_bb *)
      let end_bb = L.append_block context "range_end" the_function in
      let entry_builder = L.builder_at_end context entry_bb in
      let curr_val = L.build_load iterator "load_iter" entry_builder in (* in entry_bb, load value for iterator on stack *)
      let cond = L.build_icmp L.Icmp.Sge curr_val end_val "range_cmp" entry_builder in (* then check if it equals end_val *)
      ignore(L.build_cond_br cond end_bb body_bb entry_builder); (* conditional branch at end of entry_bb *)
      L.builder_at_end context end_bb
    | SIRange(v1, v2, body) ->
      let (t, n) = (match v1 with
        | SBind(t1, n1) -> (t1, n1)
        | _ -> raise (Failure ("invalid irange declaration")))
      in
      let start_val = L.const_int i32_t 0 in
      let iterator = L.build_alloca i32_t "iter" builder in
      Hashtbl.add local_vars n iterator;
      ignore(L.build_store start_val iterator builder);
      let end_val = (match v2 with
        | (List(ty), SId(s)) ->
          let pointer = lookup s in
          (match ty with
            | Int | Bool -> L.build_call int_list_size [| pointer |] "size" builder
            | String | Char -> L.build_call str_list_size [| pointer |] "size" builder
            | Float -> L.build_call float_list_size [| pointer |] "size" builder
            | _ -> raise (Failure ("invalid list type")))
        | (Array(ty, _), SId(s)) ->
          let pointer = lookup s in
          (match ty with
            | Int | Bool -> L.build_call int_arr_size [| pointer |] "size" builder
            | String | Char -> L.build_call str_arr_size [| pointer |] "size" builder
            | Float -> L.build_call float_arr_size [| pointer |] "size" builder
            | _ -> raise (Failure ("invalid list type")))
        | _ -> raise (Failure ("invalid irange declaration")))
      in
      let entry_bb = L.append_block context "irange_entry" the_function in
      ignore(L.build_br entry_bb builder);
      ignore(L.position_at_end entry_bb builder);
      let body_bb = L.append_block context "irange_body" the_function in
      let body_builder = build_body (L.builder_at_end context body_bb) the_function var_map body in
      let step_bb = L.append_block context "irange_step" the_function in
      ignore(L.build_br step_bb body_builder);
      let step_builder = L.builder_at_end context step_bb in
      let tmp_val = L.build_load iterator "load_iter" step_builder in
      let next_val = L.build_add tmp_val (L.const_int i32_t 1) "next_val" step_builder in
      ignore(L.build_store next_val iterator step_builder);
      ignore(L.build_br entry_bb step_builder);
      let end_bb = L.append_block context "range_end" the_function in
      let entry_builder = L.builder_at_end context entry_bb in
      let curr_val = L.build_load iterator "load_iter" entry_builder in
      let cond = L.build_icmp L.Icmp.Sge curr_val end_val "irange_cmp" entry_builder in
      ignore(L.build_cond_br cond end_bb body_bb entry_builder);
      L.builder_at_end context end_bb
  	| SDo(body, e) ->
      let do_bb = L.append_block context "do_body" the_function in (* create main loop body block *)
      ignore(L.build_br do_bb builder); (* force it to execute at least once *)
      let body_builder = build_body (L.builder_at_end context do_bb) the_function var_map body in
      let while_bb = L.append_block context "dowhile_cond" the_function in
      let end_bb = L.append_block context "do_end" the_function in
      let while_builder = L.builder_at_end context while_bb in
      let cond = build_expr while_builder e in
      ignore(L.build_cond_br cond do_bb end_bb while_builder); (* conditional branch at end of while_bb to either do_bb or end_bb *)
      ignore(L.build_br while_bb body_builder); (* branch at end of do_bb to while_bb *)
      L.builder_at_end context end_bb
  	| SReturn(e) -> ignore(L.build_ret (build_expr builder e) builder); builder
  	| SAssign(s, e) ->
      (match s with
        | (ty, SId(name)) ->
          (match ty with
            | List(t) ->
              let pointer = lookup name in
              (match e with
                | (_, SListLit(l)) -> ignore(build_list_lit pointer builder l); builder
                | (List(ty), SId(s)) ->
                  let p2 = lookup s in
                  (match ty with
                    | Int | Bool -> ignore(L.build_call copy_int_list [| p2 ; pointer |] "" builder); builder
                    | String | Char -> ignore(L.build_call copy_str_list [| p2 ; pointer |] "" builder); builder
                    | Float -> ignore(L.build_call copy_float_list [| p2 ; pointer |] "" builder); builder
                    | _ -> raise (Failure "invalid assignment on type list"))
                | _ -> raise (Failure ("invalid assignment on type list")))
            | Array(t, sz) ->
              let pointer = lookup name in
              (match e with
                | (_, SArrayLit(l)) -> ignore(build_arr_lit pointer builder 0 l); builder
                | (Array(ty2, sz2), SId(s)) ->
                  let p2 = lookup s in
                  (match ty2 with
                    | Int | Bool -> ignore(L.build_call copy_int_arr [| p2 ; pointer |] "" builder); builder
                    | String | Char -> ignore(L.build_call copy_str_arr [| p2 ; pointer |] "" builder); builder
                    | Float -> ignore(L.build_call copy_float_arr [| p2 ; pointer |] "" builder); builder
                    | _ -> raise (Failure ("invalid assignment on type array")))
                | _ -> raise (Failure ("invalid assignment on type array")))
            | _ ->
              let e' = build_expr builder e in
              ignore(L.build_store e' (lookup name) builder); builder)
        | (ty, SAccess(e1, e2)) ->
          let (t, s) = (match e1 with
            | (t1, SId(s1)) -> (t1, s1)
            | _ -> raise (Failure ("invalid assignment on non id")))
          in
          let pointer = lookup s in
          let idx = build_expr builder e2 in
          let v = build_expr builder e in
          (match t with
            | List(x) ->
              (match ty with
                | Int -> ignore(L.build_call assign_int [| pointer ; idx ; v |] "" builder); builder
                | Bool -> ignore(L.build_call assign_int [| pointer ; idx ; (L.const_intcast v i32_t false) |] "" builder); builder
                | String | Char -> ignore(L.build_call assign_str [| pointer ; idx ; v |] "" builder); builder
                | Float -> ignore(L.build_call assign_float [| pointer ; idx ; v |] "" builder); builder
                | _ -> raise (Failure ("invalid access assignment on type list")))
            | Array(t1, x) ->
              (match t1 with
                | Int -> ignore(L.build_call assign_int_arr [| pointer ; idx ; v |] "" builder); builder
                | Bool -> ignore(L.build_call assign_int_arr [| pointer ; idx ; (L.const_intcast v i32_t false) |] "" builder); builder
                | String | Char -> ignore(L.build_call assign_str_arr [| pointer ; idx ; v |] "" builder); builder
                | Float -> ignore(L.build_call assign_float_arr [| pointer ; idx ; v |] "" builder); builder
                | _ -> raise (Failure ("invalid access assignment on type array")))
            | _ -> raise (Failure ("invalid access assignment")))
          | _ -> raise (Failure ("invalid assignment")))
  	| SDecAssign(s, e) ->
      let (ty, id) = match s with
        | SBind(t, e) -> (t, e)
        | _ -> raise (Failure ("invalid declaration"))
      in
      (match ty with
        | List(t1) ->
          let _ = build_stmt builder the_function var_map s in
          let pointer = lookup id in
          (match e with
            | (_, SListLit(l)) -> ignore(build_list_lit pointer builder l); builder
            | (List(ty2), SId(s)) ->
              let p2 = lookup s in
              (match ty2 with
                | Int | Bool -> ignore(L.build_call copy_int_list [| p2 ; pointer |] "" builder); builder
                | String | Char -> ignore(L.build_call copy_str_list [| p2 ; pointer |] "" builder); builder
                | Float -> ignore(L.build_call copy_float_list [| p2 ; pointer |] "" builder); builder
                | _ -> raise (Failure ("invalid assignment on type list")))
            | _ -> raise (Failure ("invalid assignment on type list")))
        | Array(t1, sz) ->
          let _ = build_stmt builder the_function var_map s in
          let pointer = lookup id in
          (match e with
            | (Array(t2, _), SId(s)) ->
              let p2 = lookup s in
              (match t2 with
                | Int | Bool -> ignore(L.build_call copy_int_arr [| p2 ; pointer |] "" builder); builder
                | String | Char -> ignore(L.build_call copy_str_arr [| p2 ; pointer |] "" builder); builder
                | Float ->  ignore(L.build_call copy_float_arr [| p2 ; pointer |] "" builder); builder
                | _ -> raise (Failure ("invalid assignment on type array")))
            | _ -> raise (Failure ("invalid assignment on type array")))
        | _ ->
          let e' = build_expr builder e in
          let pointer = L.build_alloca (ltype_of_typ ty) id builder in
          Hashtbl.add var_map id pointer;
          ignore(L.build_store e' pointer builder); builder) (* build store function to load value into register *)
    | SArrayAssign(s, e_lst) ->
      let (ty, id) = match s with
        | SBind(t, e) -> (t, e)
        | _ -> raise (Failure ("invalid array declaration and assignment"))
      in
      (match ty with
        | Array(t, _) ->
          let _ = build_stmt builder the_function var_map s in
          let pointer = lookup id in
          ignore(build_arr_lit pointer builder 0 e_lst);
          builder
        | _ -> raise (Failure ("invalid array declaration and assignment")))
    | SPrint(e) ->
      (match e with
        | (Int, _) ->
          let e' = build_expr builder e in
          ignore(L.build_call printf_func [| int_format_str ; e' |] "printf" builder);
          builder
        | (String, _) ->
          let e' = build_expr builder e in
          ignore(L.build_call printf_func [| str_format_str ; e' |] "print_str" builder);
          builder
        | (Bool, _) ->
          let e' = build_expr builder e in
          ignore(L.build_call printf_func [| bool_format_str ; e' |] "print_bool" builder);
          builder
        | (Float, _) ->
          let e' = build_expr builder e in
          ignore(L.build_call printf_func [| float_format_str ; e' |] "print_float" builder);
          builder
        | (Char, _) ->
          let e' = build_expr builder e in
          ignore(L.build_call printf_func [| char_format_str ; e' |] "print_char" builder);
          builder
        | (List(t), SId(s)) ->
          let pointer = lookup s in
          (match t with
            | Int | Bool -> ignore(L.build_call print_int_list [| pointer |] "" builder); builder
            | Float -> ignore(L.build_call print_float_list [| pointer |] "" builder); builder
            | String | Char -> ignore(L.build_call print_str_list [| pointer |] "" builder); builder
            | _ -> raise (Failure ("invalid print call on list type")))
        | (Array(t, sz), SId(s)) ->
          let pointer = lookup s in
          (match t with
            | Int | Bool -> ignore(L.build_call print_int_arr [| pointer |] "" builder); builder
            | Float -> ignore(L.build_call print_float_arr [| pointer |] "" builder); builder
            | String | Char -> ignore(L.build_call print_str_arr [| pointer |] "" builder); builder
            | _ -> raise (Failure ("invalid print call on array type")))
        | _ -> raise (Failure ("invalid print call")))
    | SAppend(e1, e2) ->
      let (t, s) = (match e1 with
        | (List(t1), SId(s1)) -> (t1, s1)
        | _ -> raise (Failure ("cannot call append on non list type")))
      in
      let pointer = lookup s in
      let value = build_expr builder e2 in
      (match t with
        | Int -> ignore(L.build_call append_int [| pointer ; value |] "" builder); builder
        | Bool -> ignore(L.build_call append_int [| pointer ; (L.const_intcast value i32_t false) |] "" builder); builder
        | Float -> ignore(L.build_call append_float [| pointer ; value |] "" builder); builder
        | String | Char -> ignore(L.build_call append_str [| pointer ; value |] "" builder); builder
        | _ -> raise (Failure ("invalid type in append call")))
    | SRemove(e1, e2) ->
      let (t, s) = (match e1 with
        | (List(t1), SId(s1)) -> (t1, s1)
        | _ -> raise (Failure ("cannot call remove on non list type")))
      in
      let pointer = lookup s in
      let idx = build_expr builder e2 in
      (match t with
        | Int | Bool -> ignore(L.build_call remove_int [| pointer ; idx |] "" builder); builder
        | Float -> ignore(L.build_call remove_float [| pointer ; idx |] "" builder); builder
        | String | Char -> ignore(L.build_call remove_str [| pointer ; idx |] "" builder); builder
        | _ -> raise (Failure ("invalid type in remove call")))
    | SInsert(e1, e2, e3) ->
      let (t, s) = (match e1 with
        | (List(t1), SId(s1)) -> (t1, s1)
        | _ -> raise (Failure ("cannot insert on non list type")))
      in
      let pointer = lookup s in
      let idx = build_expr builder e2 in
      let v = build_expr builder e3 in
      (match t with
        | Int -> ignore(L.build_call insert_int [| pointer ; idx ; v |] "" builder); builder
        | Bool -> ignore(L.build_call insert_int [| pointer ; idx ; (L.const_intcast v i32_t false) |] "" builder); builder
        | Float -> ignore(L.build_call insert_float [| pointer ; idx ; v |] "" builder); builder
        | String | Char -> ignore(L.build_call insert_str [| pointer ; idx ; v |] "" builder); builder
        | _ -> raise (Failure ("invalid type for list element")))
  	| SCont -> raise (Failure ("continue not yet implemented"))
  	| SBreak -> raise (Failure ("break not yet implemented"))
  	| SPass -> raise (Failure ("pass not yet implemented"))
    | _ -> raise (Failure ("invalid statement, something is wrong..."))

  in

  let rec build_all_stmts b the_function var_map = function
    | [] -> b
    | _ as st :: tail ->
      let b' = build_stmt b the_function var_map st in
      build_all_stmts b' the_function var_map tail
  in
  let final_builder = build_all_stmts builder main_function global_vars stmts in
  ignore (L.build_ret (L.const_int i32_t 0) final_builder);

  the_module
