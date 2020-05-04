type action = Ast | Sast | LLVM_IR

module StringMap = Map.Make(String)

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
    let speclist = [
      ("-a", Arg.Unit (set_action Ast), "Print the AST");
      ("-s", Arg.Unit (set_action Sast), "Print the SAST");
      ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ] in
    let usage_msg = "usage: ./rattle.native [-a|-s|-l] [file.rs]" in
    let channel = ref stdin in
    Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

    let lexbuf = Lexing.from_channel !channel in

    let ast = Parser.program Scanner.token lexbuf in

    match !action with
      | Ast -> raise (Failure ("not yet impemented"))
      | _ -> let sast = Semant.check ast StringMap.empty StringMap.empty in
        match !action with
          | Ast -> raise (Failure ("not yet impemented"))
          | Sast -> raise (Failure ("not yet impemented"))
          | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate sast))
