(* The scanner for SIMPLE language *)

{
 open Parser  
 exception No_such_symbol
 let line = ref 1;;
}

let digit = ['0'-'9']
let id = ['a'-'z' '_'] ['a'-'z' '0'-'9']*

rule lexer = parse
| digit+ as num  { NUM (int_of_string num) }
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
| '>'      { GT }
| '<'      { LT }
| ">="     { GE }
| "<="     { LE }
| '+'      { ADD }
| '-'      { SUB }
| '*'      { MUL }
| '/'      { DIV }
| '{'      { LB  }
| '}'      { RB  }
| '('      { LP  }
| ')'      { RP  }
| ','      { COMMA }
| ';'      { SEMI }
|  '\n'       { (line := (!line) + 1; lexer lexbuf) }(* eat up whitespace *) 
| [' ' '\t'] { lexer lexbuf }(* eat up whitespace *) 
| eof { raise End_of_file }
| _        { raise No_such_symbol }
