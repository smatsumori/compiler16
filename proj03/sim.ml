let main () =
  (* The open of a file *)
  let cin =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel cin in
  (* The start of the entire program *)
  (*  Absyn.print_stmt (Parser.prog Lexer.lexer lexbuf);; *)

    let output = open_out "tmp.s" in                           (* 生成コード用ファイルtmp.sをオープン *)
       let code = Emitter.emit_prog (Parser.prog Lexer.lexer lexbuf) in                 (* コード生成 *)
         (output_string output code); close_out output;     (* 生成コードの書出しとファイルのクローズ *)
           let _ = Unix.system "gcc tmp.s" in () ;;                (* アセンブラとリンカの呼出し *)

let _ = try main () with 
         Parsing.Parse_error 
             -> Printf.printf "Syntax error!\n"
       | Emitter.No_such_variable x -> Printf.printf "No such symbol: \"%s\"\n" x;;
