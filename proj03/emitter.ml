open Absyn
open Printf

let label = ref 0
let incLabel() = (label := !label+1; !label)

type varInfo = {offset: int; vLevel: int}
type funInfo = {formals: int; fLevel: int}
type enventry = VarEntry of varInfo | FunEntry of funInfo


exception Compiler_error
(* µ­¹æÉ½ *)
exception No_such_variable of string
let initTable = function x -> raise (No_such_variable x)
let update var vl t = function x -> if x = var then vl else t x

(* str ¤ò n ²ó¥³¥Ô¡¼¤¹¤ë *)
let rec nCopyStr n str =
    if n > 0 then str ^ (nCopyStr (pred n) str) else ""


(* ¸Æ½Ð¤·»þ¤Ëcallee¤ËÅÏ¤¹ÀÅÅª¥ê¥ó¥¯ *)
let passLink src dst = 
  if src >= dst then 
    let deltaLevel = src-dst+1 in
      "\tmovq %rbp, %rbx\n"
     ^ nCopyStr deltaLevel "\tmovq 16(%rbx), %rbx\n"
     ^ "\tpushq %rbx\n"
  else
    "\tpushq %rbp\n"   

(* printf¤äscanf¤Ç»È¤¦Ê¸»úÎó *)
let io =  "IO:\n\t.string \"%lld\"\n"
            ^ "\t.text\n"
(* main´Ø¿ô¤ÎÆ¬ *)
let header =  "\t.globl main\n"
            ^ "main:\n"
            ^ "\tpushq %rbp\n"        (* ¥Õ¥ì¡¼¥à¥Ý¥¤¥ó¥¿¤ÎÊÝÂ¸ *)
            ^ "\tmovq %rsp, %rbp\n"   (* ¥Õ¥ì¡¼¥à¥Ý¥¤¥ó¥¿¤ò¥¹¥¿¥Ã¥¯¥Ý¥¤¥ó¥¿¤Î°ÌÃÖ¤Ë *)
(* ´Ø¿ô¸Æ½Ð¤·¤«¤é¤ÎÌá¤ê½èÍý *)
let footer = "\tleaveq\n"  (* -> movq %rbp, %rsp; popq %rbp *)
            ^ "\tretq\n" (* ¸Æ½Ð¤·°ÌÃÖ¤Î¼¡¤Î¥¢¥É¥ì¥¹¤ØÌá¤ë *)

(* ¼Â°ú¿ô¤Ï¡¤%rbp ¤«¤é +24 ¤Î¤È¤³¤í¤Ë¤¢¤ë¡¥*)
let savedARG = 24 (* return address,  static link, old %rbp *)

(* ¥Ö¥í¥Ã¥¯¤Î½èÍý *)
let rec emit_block dl sl nest addr env = 
      (* ¥Ö¥í¥Ã¥¯ÆâÀë¸À¤Î½èÍý *)
      let (dEnv,dAddr,dCode) = 
         List.fold_left (fun (env,addr,code) d  -> 
                           let (lenv,laddr,lcode) = emit_dec d nest (addr) env in
                             (lenv,laddr,code^lcode)) (env,0,"") dl 
      in  (* ¥Õ¥ì¡¼¥à¤Î³äÉÕ¤± *)
          let fCode = sprintf "\tsubq $%d, %%rsp\n" ((-dAddr+16)/16*16) in
      (* ËÜÂÎ¡ÊÊ¸Îó¡Ë¤Î¥³¡¼¥ÉÀ¸À® *)
             let body = List.fold_left (fun str ast -> (str ^ emit_stmt ast nest dAddr dEnv)) "" sl
                     (* Àë¸À¤Î¥³¡¼¥É¤ÈËÜÂÎ¤Î¥³¡¼¥É¤òÊÌ¡¹¤ËÊÖ¤¹¡¥*)
                     in (dCode, fCode ^ body)
(* Àë¸ÀÉô¤Î½èÍý¡§ÊÑ¿ôÀë¸À->µ­¹æÉ½¤Ø¤Î³ÊÇ¼¡¤´Ø¿ôÄêµÁ->¶É½êÀë¸À¤Î½èÍý¤È¥³¡¼¥ÉÀ¸À® *)
and emit_dec ast nest addr env = match ast with
   (* ´Ø¿ôÄêµÁ¤Î½èÍý *)
   FunctionDec (s, l, Block (dl,sl)) -> 
    (* ´Ø¿ôÌ¾¤Îµ­¹æÉ½¤Ø¤ÎÅÐÏ¿ *)
    let funEnv = update s (FunEntry {formals=(List.length l); fLevel=nest+1}) env in
      (* ²¾°ú¿ô¤Îµ­¹æÉ½¤Ø¤ÎÅÐÏ¿ *)
      let (sigEnv,_) =  
         List.fold_left (fun (env,addr) (s,t) -> 
                            (update s (VarEntry {offset=addr; vLevel=nest+1}) env, addr+8)) (funEnv,savedARG) l 
      in (* ´Ø¿ôËÜÂÎ¡Ê¥Ö¥í¥Ã¥¯¡Ë¤Î½èÍý *)
         let (dcode, bcode) = emit_block dl sl (nest+1) 0 sigEnv in
    (* ´Ø¿ô¥³¡¼¥É¤Î¹çÀ® *)
              ( funEnv, addr, sprintf "%s:\n" s      (* ´Ø¿ô¥é¥Ù¥ë *)
                              ^ "\tpushq %rbp\n"       (* ¥Õ¥ì¡¼¥à¥Ý¥¤¥ó¥¿¤ÎÊÝÂ¸ *)
                              ^ "\tmovq %rsp, %rbp\n"  (* ¥Õ¥ì¡¼¥à¥Ý¥¤¥ó¥¿¤Î¥¹¥¿¥Ã¥¯¥Ý¥¤¥ó¥¿°ÌÃÖ¤Ø¤Î°ÜÆ° *)
                              ^ bcode                  (* ËÜÂÎ¥³¡¼¥É *)
                              ^ footer                 (* Ìá¤ê¥³¡¼¥É *)
                              ^ dcode )                (* ¶É½ê´Ø¿ô¥³¡¼¥É *)
   (* ÊÑ¿ôÀë¸À¤Î½èÍý *)
 | VarDec (s,t) -> (update s (VarEntry {offset=addr-8; vLevel=nest}) env, addr-8, "")
 | _ -> raise Compiler_error

(* Ê¸¤Î½èÍý *)
and emit_stmt ast nest addr env = 
                 match ast with
                   (* scan¤Î¥³¡¼¥É *)
                   | CallProc ("scan", [VarExp s]) ->
                       let entry = env s in
                        (match entry with
                           VarEntry {offset=addr; vLevel=level} -> 
                               "\tmovq %rbp, %rax\n" 
                             ^ nCopyStr (nest-level) "\tmovq 16(%rax), %rax\n"
                             ^ ((sprintf "\tleaq %d(%%rax), %%rsi\n" addr)
                             ^ "\tleaq IO(%rip), %rdi\n"
                             ^ "\tmovq $0, %rax\n"
                             ^ "\tcallq scanf\n")
                       | _ -> "")
                   (* iprint¤Î¥³¡¼¥É *)
                   | CallProc ("iprint", [arg]) -> 
                           (emit_exp arg nest env
                        ^  "\tpopq  %rsi\n"
                        ^  "\tleaq IO(%rip), %rdi\n"
                        ^  "\tmovq $0, %rax\n"
                        ^  "\tcallq printf\n")
                   (* sprint¤Î¥³¡¼¥É *)
                   | CallProc ("sprint", [StrExp s]) -> 
                       (let l = incLabel() in
                              ("\t.data\n"
                            ^ sprintf "L%d:\t.string %s\n" l s
                            ^ "\t.text\n"
                            ^ sprintf "\tleaq L%d(%%rip), %%rdi\n" l 
                            ^  "\tmovq $0, %rax\n"     
                            ^ "\tcallq printf\n"))
                  (* return¤Î¥³¡¼¥É *)
                  | CallProc ("return", head::l) ->
                              emit_exp head nest env
                            ^ "\tpopq %rax\n"
                  (* ¼êÂ³¤­¸Æ½Ð¤·¤Î¥³¡¼¥É *)
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
                  (* ÂåÆþ¤Î¥³¡¼¥É¡§ÂåÆþÀè¥Õ¥ì¡¼¥à¤òsetVar¤Çµá¤á¤ë¡¥*)
                  | Assign (s, e) -> (let entry = env s in (* µ­¹æÉ½¤«¤éÂåÆþÀèÊÑ¿ô¤ò¼è¤ê½Ð¤¹¡¥*)
                                        match entry with 
                                           VarEntry {offset=addr; vLevel=level} -> 
                                                emit_exp e nest env (* ±¦ÊÕ¤Î¼°¤Î½èÍý *)
                                              ^ "\tmovq %rbp, %rax\n" 
                                              ^ nCopyStr (nest-level) "\tmovq 16(%rax), %rax\n"
                                              ^ sprintf "\tpopq %d(%%rax)\n" addr
                                         | FunEntry _ -> "")
                  (* else¤Ê¤·ifÊ¸¤Î¥³¡¼¥É *)
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
(* ¼°¤Î½èÍý *)
and emit_exp ast nest env = match ast with
                  (* ÊÑ¿ô»²¾È¤Î¥³¡¼¥É¡§reVar¤Ç»²¾È¥Õ¥ì¡¼¥à¤òµá¤á¤ë *)
                    VarExp s -> (let entry = env s in 
                        match entry with
                            VarEntry {offset=addr; vLevel=level} -> 
                                  "\tmovq %rbp, %rax\n" 
                                ^ nCopyStr (nest-level) "\tmovq 16(%rax), %rax\n"
                                ^ sprintf "\tpushq %d(%%rax)\n" addr
                          | _ -> "")
                  (* À°¿ôÄê¿ô¤Î¥³¡¼¥É *)
                  |  IntExp i -> (sprintf "\tpushq $%d\n" i)
                  (* ÊÑ¿ô»²¾È¤Î¥³¡¼¥É¡§reVar¤Ç»²¾È¥Õ¥ì¡¼¥à¤òµá¤á¤ë *)
                  (* +¤Î¥³¡¼¥É *)
                  | CallFunc ("+", [left; right]) -> 
                                             emit_exp left nest env
                                           ^ emit_exp right nest env
                                           ^ "\tpopq %rax\n"
                                           ^ "\taddq %rax, (%rsp)\n"
                  (* -¤Î¥³¡¼¥É *)
                  | CallFunc ("-", [left; right]) ->
                                             emit_exp left nest env
                                           ^ emit_exp right nest env
                                           ^ "\tpopq %rax\n"
                                           ^ "\tsubq %rax, (%rsp)\n"
                  (* *¤Î¥³¡¼¥É *)
                  | CallFunc ("*", [left; right]) ->
                                             emit_exp left nest env
                                           ^ emit_exp right nest env
                                           ^ "\tpopq %rax\n"
                                           ^ "\timulq (%rsp), %rax\n"
                                           ^ "\tmovq %rax, (%rsp)\n"
                  (* /¤Î¥³¡¼¥É *)
                  | CallFunc ("/", [left; right]) ->
                                             emit_exp left nest env
                                           ^ emit_exp right nest env
                                           ^ "\tpopq %rbx\n"
                                           ^ "\tpopq %rax\n"
                                           ^ "\tcqto\n"
                                           ^ "\tidivq %rbx\n"
                                           ^ "\tpushq %rax\n"
                  (* È¿Å¾¤Î¥³¡¼¥É *)
                  | CallFunc("!",  arg::_) -> 
                                             emit_exp arg nest env
                                           ^ "\tnegq (%rsp)\n"
                  (* ´Ø¿ô¸Æ½Ð¤·¤Î¥³¡¼¥É *)
                  | CallFunc (s, el) -> 
                                 emit_stmt (CallProc(s, el)) nest 0 env 
                                 (* ÊÖÌáÃÍ¤Ï%rax¤ËÆþ¤ì¤ÆÊÖ¤¹ *)
                               ^ "\tpushq %rax\n"
                  | _ -> ""

(* ´Ø·¸±é»»¤Î½èÍý *)
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
(* ¥×¥í¥°¥é¥àÁ´ÂÎ¤ÎÀ¸À® *)
let emit_prog ast = match ast with
                       Block (dl, sl) -> 
                           let (dcode, bcode) = emit_block dl sl 0 0 initTable in
                                io ^ header ^ bcode ^ footer ^ dcode
                     | otherAst -> 
                           let scode = emit_stmt otherAst 0 0 initTable in
                                io ^ header ^ scode ^ footer
