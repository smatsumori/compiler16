let main () =
  (* The open of a file *)
  let cin =
    if Array.length Sys.argv > 1    (* no file setted *)
    then open_in Sys.argv.(1)   (* 0 is command itself *)
    else stdin  (* if nofile read from stdin *)
  in
  let lexbuf = Lexing.from_channel cin in
  let rec parse_buf_exn lexbuf = 
      try 
          let result = Parser.prog Lexer.lexer lexbuf in
          Absyn.print_stmt result; print_newline(); flush stdout;
      with
      |End_of_file ->
              print_string "Finished\n";
              exit 0
      |Lexer.No_such_symbol ->
              print_string "No such symbol"
      |Parsing.Parse_error ->
              begin
                  let curr = lexbuf.Lexing.lex_curr_p in
                  let line = (!Lexer.line) in
                  let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
                  let tok = Lexing.lexeme lexbuf in
                  (* Lexing.flush_input lexbuf; *)
                  Printf.printf "Syntax error found on:\nL: %d, C:%d, TOK: %s\n" line cnum tok;
                  parse_buf_exn lexbuf
              end
    in
    parse_buf_exn lexbuf
;;
 
let _ = main()
