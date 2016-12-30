let main () =
  (* The open of a file *)
  let cin =
    if Array.length Sys.argv > 1    (* no file setted *)
    then open_in Sys.argv.(1)   (* 0 is command itself *)
    else stdin  (* if nofile read from stdin *)
  in
    let lexbuf = Lexing.from_channel cin in
  (* The start of the entire program *)
  while true do
    try
        let result = Parser.prog Lexer.lexer lexbuf in
        Absyn.print_stmt result; print_newline(); flush stdout;
    with 
    |Parsing.Parse_error
     -> print_string "Syntax error found on:\nLine: ";
     print_int (Lexing.(lexbuf.lex_curr_p.pos_lnum));
     print_string(" Token: ");
     print_string (Lexing.lexeme lexbuf);
     print_string "\n";
     flush stdout;
    |Lexer.No_such_symbol ->
     print_string "Error Symbol";
    |End_of_file ->
     exit 0
    done
;;
  (*  -> print_int lexbuf.lex_curr_p.pos_lnum *)

  (* print_stmt prints syntax tree *)

(* catch main get parse_error *)
 
let _ = main()

(*
let _ = try main () with Parsing.Parse_error 
          -> print_string "Syntax error detected!\n"
*)

