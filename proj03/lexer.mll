(* The scanner for SIMPLE language *)

{
 open Parser  
 exception No_such_symbol
}

let digit = ['0'-'9']
let id = ['a'-'z' '_'] ['a'-'z' '0'-'9']*

rule lexer = parse
| digit+ as num  { NUM (int_of_string num) }
| "do"     { DO }   (* HERE do while*)
| "for"    { FOR }    (* HERE for loop *)
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "scan"   { SCAN }
| "sprint" { SPRINT }
| "iprint" { IPRINT }
| "int"    { INT }
| "return" { RETURN }
| id as text { ID text }
| '\"'[^'\"']*'\"' as str { STR str }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| "++"     { INC }  (* HERE  increment*)
| "+="     { ADDA } (* HERE *)
| '>'      { GT }
| '<'      { LT }
| ">="     { GE }
| "<="     { LE }
| '+'      { ADD }
| '-'      { SUB }
| '*'      { MUL }
| '/'      { DIV }
| '%'      { MOD }  (* HERE modular *)
| '^'      { POW }  (* HERE  power*)
| '{'      { LB  }
| '}'      { RB  }
| '('      { LP  }
| ')'      { RP  }
| ','      { COMMA }
| ';'      { SEMI }
| ['\n' ' ' '\t'] { lexer lexbuf }(* eat up whitespace *) 
| eof { raise End_of_file }
| _        { raise No_such_symbol }
