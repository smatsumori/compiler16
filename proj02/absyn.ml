open Printf;;

(* The definition of the abstract syntax tree *)
type symbol = string;;
type typ = string;;
(* of sth : sth is arg *)
type exp = VarExp of symbol | StrExp of string | IntExp of int 
        | CallFunc of symbol * (exp list) 
and dec = FunctionDec of symbol * ((symbol*typ) list) * stmt    (* Def function, (... list): args *)
        | VarDec of symbol * typ
and stmt = CallProc of symbol * (exp list)
        | Block of (dec list) * (stmt list)
        | Assign of symbol * exp
        | If of exp * stmt * (stmt option)  (* if else stmt *)
        | While of exp * stmt
        | NilStmt;;

(* Below is a test code *)
(* The print of the abstract syntax tree *)
let rec  print_stmt ast = match ast with
                    CallProc (s, l) -> (printf " CallProc(["; List.iter  print_exp l; printf "])")
                  | Block (dl, sl) -> (printf " Block(["; List.iter print_dec dl;print_string "],[";
                                       List.iter print_stmt sl; print_string "])")
                  | Assign (s, e) -> (printf " Assign(%s," s; print_exp e; printf ")")
                  | If (e,s,None) -> (printf " If("; print_exp e; print_stmt s; printf ")")
                  | If (e,s1,Some s2) -> (printf " If("; print_exp e; print_string ",";
                                                    print_stmt s1; print_string ",";
                                                    print_stmt s2; printf ")")
                  | While (e,s) -> (printf " While("; print_exp e; print_string ","; 
                                                    print_stmt s; print_string ")")
                  | NilStmt -> printf " NilStmt"
and print_dec ast = match ast with
                    FunctionDec (s, l, b) -> 
                        (printf " FunctionDec(%s," s; print_string "["; 
                             List.iter (fun (s,t) -> printf " %s:%s " s t) l;
                             print_string "],";
                            print_stmt b; print_string")")
                  | VarDec (t,s) -> (printf " VarDec("; printf "%s:%s)" s t)
and print_exp ast = match ast with
                    VarExp s -> printf " VarExp(%s)" s
                  | StrExp s -> printf " StrExp(%s)" s
                  | IntExp i -> printf " IntExp(%d)" i
                  | CallFunc (s, l) -> (printf " CallFunc(%s,[" s; List.iter print_exp l; print_string "])")

