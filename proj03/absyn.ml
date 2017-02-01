open Printf;;

(* The definition of the abstract syntax tree *)
type symbol = string;;
type typ = string;;
type exp = VarExp of symbol | StrExp of string | IntExp of int 
        | CallFunc of symbol * (exp list)
and dec = FunctionDec of symbol * ((symbol*typ) list) * stmt
        | VarDec of symbol * typ
and stmt = CallProc of symbol * (exp list)
        | Block of (dec list) * (stmt list)
        | Assign of symbol * exp
        | AddAssign of symbol * exp
        | If of exp * stmt * (stmt option)
        | While of exp * stmt
        | NilStmt;;

(* The print of the abstract syntax tree *)
let rec  print_stmt ast = match ast with
                    CallProc (s, pl) -> (printf "CallProc(\"%s\",[" s; 
                               (match pl with 
                                    [] -> ()
                                  | (e::l) -> (print_exp e; List.iter  (fun e -> printf "; "; print_exp e) l)); 
                               printf "])")
                  | Block (dl, sl) -> (printf "Block(["; 
                               (match dl with 
                                    [] -> ()
                                  | (d::l) -> (print_dec d; List.iter (fun d -> printf "; "; print_dec d) l));
                               print_string "],[";
                               (match sl with
                                    [] -> ()
                                  | (s::l) -> (print_stmt s; List.iter (fun s -> printf ";"; print_stmt s) l)); 
                               print_string "])")
                  | Assign (s, e) -> (printf "Assign(\"%s\"," s; print_exp e; printf ")")
                  | AddAssign (s, e) -> (printf "AddAssign(\"%s\"," s; print_exp e; printf ")")   (* AddAssign *)
                  | If (e,s,None) -> (printf "If("; print_exp e; print_stmt s; printf ")")
                  | If (e,s1,Some s2) -> (printf "If("; print_exp e; print_string ",";
                                                    print_stmt s1; print_string ",";
                                                    print_stmt s2; printf ")")
                  | While (e,s) -> (printf "While("; print_exp e; print_string ","; 
                                                    print_stmt s; print_string ")")
                  | NilStmt -> printf "NilStmt"
and print_dec ast = match ast with
                    FunctionDec (s, pl, b) -> (printf "FunctionDec(\"%s\",[" s;
                               (match pl with 
                                     [] -> ()
                                   | ((v,t)::l) -> (printf "(\"%s\",\"%s\")" v t; List.iter (fun (v,t) -> printf "; (\"%s\",\"%s\")" v t) l));
                             print_string "],";
                             print_stmt b; print_string ")")
                  | VarDec (t,s) -> (printf " VarDec("; printf "\"%s\",\"%s\")" s t)
and print_exp ast = match ast with
                    VarExp s -> printf "VarExp(\"%s\")" s
                  | StrExp s -> printf "StrExp(%s)" s
                  | IntExp i -> printf "IntExp(%d)" i
                  | CallFunc (s, pl) -> (printf "CallFunc(\"%s\",[" s; 
                               (match pl with 
                                    [] -> ()
                                  | (e::l) -> (print_exp e; List.iter (fun e -> printf "; "; print_exp e) l));
                               print_string "])")

