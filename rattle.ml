open Printf
open Sys

type action = Ast | Sast | LLVM_IR | Exec

module StringMap = Map.Make(String)

let () =
  let action = ref Exec in
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
          | Exec ->
            let llvm_module = Llvm.string_of_llmodule (Irgen.translate sast) in
            let out = open_out "llvm.out" in
            fprintf out "%s\n" llvm_module; close_out out;
            ignore(command "llc -relocation-model=pic llvm.out");
            ignore(command "g++ llvm.out.s -o a.out");
