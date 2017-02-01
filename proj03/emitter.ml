open Absyn
open Printf

let label = ref 0
let incLabel() = (label := !label+1; !label)

type varInfo = {offset: int; vLevel: int}
type funInfo = {formals: int; fLevel: int}
type enventry = VarEntry of varInfo | FunEntry of funInfo


exception Compiler_error
(* table *)
exception No_such_variable of string
let initTable = function x -> raise (No_such_variable x)
let update var vl t = function x -> if x = var then vl else t x

(* copy string n times *)
let rec nCopyStr n str =
    if n > 0 then str ^ (nCopyStr (pred n) str) else ""


let passLink src dst = 
  if src >= dst then 
    let deltaLevel = src-dst+1 in
      "\tmovq %rbp, %rbx\n"
     ^ nCopyStr deltaLevel "\tmovq 16(%rbx), %rbx\n"
     ^ "\tpushq %rbx\n"
  else
    "\tpushq %rbp\n"   

let io =  "IO:\n\t.string \"%lld\"\n"
            ^ "\t.text\n"

let header =  "\t.globl _main\n"   (* for mac *)
            ^ "_main:\n"
            ^ "\tpushq %rbp\n"        
            ^ "\tmovq %rsp, %rbp\n"  


let footer = "\tleaveq\n"  (* -> movq %rbp, %rsp; popq %rbp *)
            ^ "\tretq\n" 

let savedARG = 24 (* return address,  static link, old %rbp *)

let rec emit_block dl sl nest addr env = 
      let (dEnv,dAddr,dCode) = 
         List.fold_left (fun (env,addr,code) d  -> 
                           let (lenv,laddr,lcode) = emit_dec d nest (addr) env in
                             (lenv,laddr,code^lcode)) (env,0,"") dl 
      in 
          let fCode = sprintf "\tsubq $%d, %%rsp\n" ((-dAddr+16)/16*16) in
             let body = List.fold_left (fun str ast -> (str ^ emit_stmt ast nest dAddr dEnv)) "" sl
                     in (dCode, fCode ^ body)
and emit_dec ast nest addr env = match ast with
   FunctionDec (s, l, Block (dl,sl)) -> 
    let funEnv = update s (FunEntry {formals=(List.length l); fLevel=nest+1}) env in
      let (sigEnv,_) =  
         List.fold_left (fun (env,addr) (s,t) -> 
                            (update s (VarEntry {offset=addr; vLevel=nest+1}) env, addr+8)) (funEnv,savedARG) l 
      in 
         let (dcode, bcode) = emit_block dl sl (nest+1) 0 sigEnv in
              ( funEnv, addr, sprintf "%s:\n" s      (* ´Ø¿ô¥é¥Ù¥ë *)
                              ^ "\tpushq %rbp\n"       (* ¥Õ¥ì¡¼¥à¥Ý¥¤¥ó¥¿¤ÎÊÝÂ¸ *)
                              ^ "\tmovq %rsp, %rbp\n"  (* ¥Õ¥ì¡¼¥à¥Ý¥¤¥ó¥¿¤Î¥¹¥¿¥Ã¥¯¥Ý¥¤¥ó¥¿°ÌÃÖ¤Ø¤Î°ÜÆ° *)
                              ^ bcode                  (* ËÜÂÎ¥³¡¼¥É *)
                              ^ footer                 (* Ìá¤ê¥³¡¼¥É *)
                              ^ dcode )                (* ¶É½ê´Ø¿ô¥³¡¼¥É *)
   (* ÊÑ¿ôÀë¸À¤Î½èÍý *)
 | VarDec (s,t) -> (update s (VarEntry {offset=addr-8; vLevel=nest}) env, addr-8, "")
 | _ -> raise Compiler_error

and emit_stmt ast nest addr env = 
                 match ast with
                   | CallProc ("scan", [VarExp s]) ->
                       let entry = env s in
                        (match entry with
                           VarEntry {offset=addr; vLevel=level} -> 
                               "\tmovq %rbp, %rax\n" 
                             ^ nCopyStr (nest-level) "\tmovq 16(%rax), %rax\n"
                             ^ ((sprintf "\tleaq %d(%%rax), %%rsi\n" addr)
                             ^ "\tleaq IO(%rip), %rdi\n"
                             ^ "\tmovq $0, %rax\n"
                             ^ "\tcallq _scanf\n")  (* DEBUG For Mac *)
                       | _ -> "")
                   | CallProc ("iprint", [arg]) -> 
                           (emit_exp arg nest env
                        ^  "\tpopq  %rsi\n"
                        ^  "\tleaq IO(%rip), %rdi\n"
                        ^  "\tmovq $0, %rax\n"
                        ^  "\tcallq _printf\n")  (* DEBUG For Mac *)
                   | CallProc ("sprint", [StrExp s]) -> 
                       (let l = incLabel() in
                              ("\t.data\n"
                            ^ sprintf "L%d:\t.string %s\n" l s
                            ^ "\t.text\n"
                            ^ sprintf "\tleaq L%d(%%rip), %%rdi\n" l 
                            ^  "\tmovq $0, %rax\n"     
                            ^ "\tcallq _printf\n"))
                  | CallProc ("return", head::l) ->
                              emit_exp head nest env
                            ^ "\tpopq %rax\n"
                  | CallProc (s, el) -> 
                      let entry = env s in 
                         (match entry with
                             (FunEntry {formals=_; fLevel=level}) -> 
                                 (* ¼Â°ú¿ô¤Î¥³¡¼¥É *)
                                 (* 16¥Ð¥¤¥È¶­³¦¤ËÄ´À° *)
                                 (if (List.length el) mod 2 = 1 then "" else "\tpushq $0\n")
                               ^ List.fold_right  (fun  ast code -> code ^ (emit_exp ast nest env)) el "" 
                                 (* ÀÅÅª¥ê¥ó¥¯¤òÅÏ¤¹¥³¡¼¥É *)
                               ^  passLink nest level
                                 (* ´Ø¿ô¤Î¸Æ½Ð¤·¥³¡¼¥É *)
                               ^  "\tcallq " ^ s ^ "\n"
                                 (* ÀÑ¤ó¤À°ú¿ô+ÀÅÅª¥ê¥ó¥¯¤ò¹ß¤í¤¹ *)
                               ^  sprintf "\taddq $%d, %%rsp\n" ((List.length el + 1 + 1) / 2 * 2 * 8) 
                            | _ -> "") 
                  (* ¥Ö¥í¥Ã¥¯¤Î¥³¡¼¥É¡§Ê¸¤òÉ½¤¹¥Ö¥í¥Ã¥¯¤Ï¡¤´Ø¿ôÄêµÁ¤òÌµ»ë¤¹¤ë¡¥*)
                  | Block (dl, sl) -> 
                      let (dcode, bcode) = emit_block dl sl nest addr env in
                          (match dcode with     (* dcode¤¬¶õÊ¸»úÎó¤Ç¤Ê¤±¤ì¤Ð¡¤´Ø¿ôÄêµÁ¤¬¤µ¤ì¤Æ¤¤¤ë¡¥*)
                                  "" -> bcode
                                 | _  -> (print_string "Functions in the sequence of statements are ignored!\n";
                                        bcode))

                  | Assign (s, e) -> (let entry = env s in  (* Get symbol from symbol table *)
                                        match entry with 
                                           VarEntry {offset=addr; vLevel=level} -> 
                                                emit_exp e nest env 
                                              ^ "\tmovq %rbp, %rax\n" 
                                              ^ nCopyStr (nest-level) "\tmovq 16(%rax), %rax\n"
                                              ^ sprintf "\tpopq %d(%%rax)\n" addr
                                         | FunEntry _ -> "")
                  (* AddAssign Here *)
                  | AddAssign (s, e) -> (let entry = env s in  (* Get symbol from symbol table (function env) *)
                                        match entry with 
                                           VarEntry {offset=addr; vLevel=level} ->  (* What is this ? *)
                                             emit_exp (CallFunc ("+", [VarExp s; e])) nest env   (* call exp and get value *)  (* adjust nest *)
                                              ^ "\tmovq %rbp, %rax\n"   (* keep rbp *)
                                              ^ nCopyStr (nest-level) "\tmovq 16(%rax), %rax\n"
                                              ^ sprintf "\tpopq %d(%%rax)\n" addr
                                         | FunEntry _ -> "")

                  | If (e,s,None) -> let (condCode,l) = emit_cond e nest env in
                                                  condCode
                                                ^ emit_stmt s nest addr env
                                                ^ sprintf "L%d:\n" l
                  (* else¤¢¤êifÊ¸¤Î¥³¡¼¥É *)
                  | If (e,s1,Some s2) -> let (condCode,l) = emit_cond e nest env in
                                            let l2 = incLabel() in 
                                                  condCode
                                                ^ emit_stmt s1 nest addr env
                                                ^ sprintf "\tjmp L%d\n" l2
                                                ^ sprintf "L%d:\n" l
                                                ^ emit_stmt s2 nest addr env 
                                                ^ sprintf "L%d:\n" l2
                  (* whileÊ¸¤Î¥³¡¼¥É *)
                  | While (e,s) -> let (condCode, l) = emit_cond e nest env in
                                     let l2 = incLabel() in
                                         sprintf "L%d:\n" l2 
                                       ^ condCode
                                       ^ emit_stmt s nest addr env
                                       ^ sprintf "\tjmp L%d\n" l2
                                       ^ sprintf "L%d:\n" l
                  (* ¶õÊ¸ *)
                  | NilStmt -> ""

(* handling expression *)
and emit_exp ast nest env = match ast with
    VarExp s -> (let entry = env s in 
        match entry with
            VarEntry {offset=addr; vLevel=level} -> 
                  "\tmovq %rbp, %rax\n" 
                ^ nCopyStr (nest-level) "\tmovq 16(%rax), %rax\n"
                ^ sprintf "\tpushq %d(%%rax)\n" addr
          | _ -> "")
  (* const integer *)
  |  IntExp i -> (sprintf "\tpushq $%d\n" i)
  | CallFunc ("^", [left; right]) ->
                             emit_exp left nest env
                           ^ emit_exp right nest env
                           ^"\tpopq %rax\n"	(* 乗数の部分を格納 *)
                           ^"\tmovq $1, %rbx\n"	(* rval *)
                           ^"LOOP:\n"
                           ^"\tcmpq $0,	%rax\n"
                           ^"\tje BREAK\n"
                           ^"\tsubq $1,	%rax\n"	
                           ^"\timulq (%rsp), %rbx\n"
                           ^"\tjmp LOOP\n"
                           ^"\tBREAK:\n"
                           ^"\tmovq %rbx, (%rsp)\n"
  | CallFunc ("+", [left; right]) -> 
                             emit_exp left nest env
                           ^ emit_exp right nest env
                           ^ "\tpopq %rax\n"
                           ^ "\taddq %rax, (%rsp)\n"
  | CallFunc ("-", [left; right]) ->
                             emit_exp left nest env
                           ^ emit_exp right nest env
                           ^ "\tpopq %rax\n"
                           ^ "\tsubq %rax, (%rsp)\n"
  | CallFunc ("*", [left; right]) ->
                             emit_exp left nest env
                           ^ emit_exp right nest env
                           ^ "\tpopq %rax\n"
                           ^ "\timulq (%rsp), %rax\n"
                           ^ "\tmovq %rax, (%rsp)\n"
  | CallFunc ("/", [left; right]) ->
                             emit_exp left nest env
                           ^ emit_exp right nest env
                           ^ "\tpopq %rbx\n"
                           ^ "\tpopq %rax\n"
                           ^ "\tcqto\n"
                           ^ "\tidivq %rbx\n"
                           ^ "\tpushq %rax\n"
  | CallFunc ("%", [left; right]) ->
                             emit_exp left nest env
                           ^ emit_exp right nest env
                           ^ "\tpopq %rbx\n"
                           ^ "\tpopq %rax\n "
                           ^ "\tcqto\n"
                           ^ "\tidivq %rbx\n"
                           ^ "\tpushq %rdx\n"   (* rdx is reminder *)
  | CallFunc("!",  arg::_) -> 
                             emit_exp arg nest env
                           ^ "\tnegq (%rsp)\n"
  | CallFunc (s, el) -> 
                 emit_stmt (CallProc(s, el)) nest 0 env 
               ^ "\tpushq %rax\n"
  | _ -> ""

(* Conditional statement *)
and emit_cond ast nest env = match ast with
                  | CallFunc (op, left::right::_) -> 
                      (let code = 
                       (* ¥ª¥Ú¥é¥ó¥É¤Î¥³¡¼¥É *)
                          emit_exp left nest env
                        ^ emit_exp right nest env
                       (* ¥ª¥Ú¥é¥ó¥É¤ÎÃÍ¤ò %rax¡¤%rbx¤Ø *)
                        ^ "\tpopq %rax\n"
                        ^ "\tpopq %rbx\n"
                       (* cmpÌ¿Îá *)                       
                        ^ "\tcmpq %rax, %rbx\n" in
                          let l = incLabel () in
                             match op with
                               (* ¾ò·ï¤ÈÊ¬´ô¤Î´Ø·¸¤Ï¡¤µÕ *)
                                "==" -> (code ^ sprintf "\tjne L%d\n" l, l)
                              | "!=" -> (code ^ sprintf "\tje L%d\n"l, l)
                              | ">"  -> (code ^ sprintf "\tjle L%d\n" l, l)
                              | "<"  -> (code ^ sprintf "\tjge L%d\n" l, l)
                              | ">=" -> (code ^ sprintf "\tjl L%d\n" l, l)
                              | "<=" -> (code ^ sprintf "\tjg L%d\n" l, l)
                              | _ -> ("",0))
                 | _ -> ("",0)

let emit_prog ast = match ast with
                       Block (dl, sl) -> 
                           let (dcode, bcode) = emit_block dl sl 0 0 initTable in
                                io ^ header ^ bcode ^ footer ^ dcode
                     | otherAst -> 
                           let scode = emit_stmt otherAst 0 0 initTable in
                                io ^ header ^ scode ^ footer
