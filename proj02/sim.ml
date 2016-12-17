let main () =
  (* The open of a file *)
  let cin =
    if Array.length Sys.argv > 1    (* no file setted *)
    then open_in Sys.argv.(1)   (* 0 is command itself *)
    else stdin  (* if nofile read from stdin *)
  in
  let lexbuf = Lexing.from_channel cin in
  (* The start of the entire program *)
  Absyn.print_stmt (Parser.prog Lexer.lexer lexbuf)
  (* print_stmt prints syntax tree *)

(* catch main get parse_error *)
let _ = try main () with Parsing.Parse_error 
          -> print_string "Syntax error!\n"

