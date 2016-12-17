type token =
  | NUM of (int)
  | STR of (string)
  | ID of (string)
  | INT
  | IF
  | WHILE
  | SPRINT
  | IPRINT
  | SCAN
  | EQ
  | NEQ
  | GT
  | LT
  | GE
  | LE
  | ELSE
  | RETURN
  | ADD
  | SUB
  | MUL
  | DIV
  | LB
  | RB
  | LP
  | RP
  | ASSIGN
  | SEMI
  | COMMA

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absyn.stmt
