open Sast

module StringMap = Map.Make(String)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let sprogram = Semant.check program StringMap.empty StringMap.empty in
  print_endline "Done"