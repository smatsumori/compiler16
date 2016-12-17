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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(*
The grammer of SIMPLE language:

prog -> stmt

ty -> int

dec -> ty ids ';'
     | ty ID '(' fargs_opt ')' block
     | empty

fargs_opt -> 
      | fargs

fargs -> ty ID
      | fargs ',' ty ID
      ;

ids -> ID
    | ids ',' ID
		
stmts -> stmt
      | stmts ';' stmt

stmt -> ID '=' expr ';'
	| IF '(' cond ')' stmt 
	| IF '(' cond ')' stmt ELSE stmt
       	| WHILE '(' cond ')' stmt
	| SPRINT '(' STRING ')'  ';'
 	| IPRINT '(' expr ')'';'
        | RETURN expr ';'
	| block
        | ';'
        | ID '(' exprs_opt ')'

block -> '{' stmts '}'

expr ->   expr '+' term
	| expr '-' term
	| term

aargs_opt -> 
        | aargs

aargs ->  aargs ',' expr
        | expr

term ->   term  '*' factor
	| term  DIV factor
	| factor

factor ->  '(' expr ')'		
	| NUM
	| ID
	| '-' expr
        | ID '(' aargs_opt ')'

cond 	-> expr condop expr

condop  -> "==" | "!=" | '>' | '<' | ">=" | "<="
*)

open Printf;;
open Absyn;;

exception Semant_error;;

# 102 "parser.ml"
let yytransl_const = [|
  260 (* INT *);
  261 (* IF *);
  262 (* WHILE *);
  263 (* SPRINT *);
  264 (* IPRINT *);
  265 (* SCAN *);
  266 (* EQ *);
  267 (* NEQ *);
  268 (* GT *);
  269 (* LT *);
  270 (* GE *);
  271 (* LE *);
  272 (* ELSE *);
  273 (* RETURN *);
  274 (* ADD *);
  275 (* SUB *);
  276 (* MUL *);
  277 (* DIV *);
  278 (* LB *);
  279 (* RB *);
  280 (* LP *);
  281 (* RP *);
  282 (* ASSIGN *);
  283 (* SEMI *);
  284 (* COMMA *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* STR *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\004\000\004\000\005\000\005\000\006\000\006\000\
\007\000\007\000\009\000\009\000\010\000\010\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\013\000\013\000\014\000\014\000\008\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\012\000\
\012\000\012\000\012\000\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\001\000\000\000\002\000\003\000\006\000\003\000\001\000\
\000\000\001\000\004\000\002\000\002\000\001\000\004\000\005\000\
\007\000\005\000\005\000\005\000\005\000\004\000\003\000\001\000\
\001\000\000\000\001\000\003\000\001\000\004\000\001\000\001\000\
\004\000\003\000\003\000\003\000\003\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\025\000\046\000\001\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\038\000\000\000\
\000\000\000\000\000\000\000\000\023\000\002\000\014\000\000\000\
\004\000\000\000\022\000\000\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\039\000\000\000\000\000\036\000\037\000\000\000\000\000\
\030\000\013\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\019\000\020\000\021\000\033\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\007\000\017\000\
\012\000\000\000\000\000\006\000\000\000\011\000"

let yydgoto = "\002\000\
\012\000\013\000\048\000\027\000\049\000\072\000\093\000\014\000\
\094\000\050\000\028\000\033\000\029\000\030\000"

let yysindex = "\016\000\
\074\255\000\000\245\254\007\255\008\255\023\255\041\255\045\255\
\042\255\000\000\000\000\000\000\000\000\000\000\042\255\042\255\
\042\255\042\255\038\255\042\255\064\255\000\000\048\255\042\255\
\042\255\066\255\051\255\009\255\049\255\047\255\153\255\149\255\
\063\255\065\255\067\255\157\255\069\255\042\255\000\000\165\255\
\042\255\042\255\042\255\042\255\000\000\000\000\000\000\073\255\
\000\000\255\254\000\000\042\255\000\000\042\255\042\255\042\255\
\042\255\042\255\042\255\074\255\074\255\062\255\068\255\070\255\
\075\255\000\000\004\255\004\255\000\000\000\000\084\255\035\255\
\000\000\000\000\009\255\009\255\009\255\009\255\009\255\009\255\
\009\255\082\255\000\000\000\000\000\000\000\000\000\000\095\255\
\000\000\106\255\074\255\112\255\089\255\088\255\000\000\000\000\
\000\000\096\255\095\255\000\000\124\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\103\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\092\255\000\000\
\000\000\000\000\000\000\014\255\000\000\107\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\103\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\111\255\130\255\000\000\000\000\043\255\000\000\
\000\000\000\000\016\255\108\255\109\255\110\255\121\255\122\255\
\125\255\001\000\000\000\000\000\000\000\000\000\000\000\126\255\
\000\000\000\000\000\000\000\000\000\000\127\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\229\255\177\255\000\000\000\000\000\000\000\000\033\000\
\000\000\000\000\250\255\119\000\115\000\000\000"

let yytablesize = 284
let yytable = "\047\000\
\016\000\003\000\026\000\004\000\005\000\006\000\007\000\008\000\
\092\000\031\000\032\000\032\000\015\000\036\000\016\000\009\000\
\001\000\039\000\040\000\101\000\010\000\073\000\074\000\043\000\
\044\000\011\000\041\000\042\000\043\000\044\000\017\000\018\000\
\082\000\083\000\067\000\068\000\069\000\070\000\029\000\035\000\
\028\000\029\000\022\000\028\000\023\000\075\000\019\000\076\000\
\077\000\078\000\079\000\080\000\081\000\003\000\046\000\004\000\
\005\000\006\000\007\000\008\000\024\000\089\000\090\000\096\000\
\020\000\025\000\037\000\009\000\021\000\008\000\008\000\038\000\
\010\000\051\000\052\000\071\000\003\000\011\000\004\000\005\000\
\006\000\007\000\008\000\041\000\042\000\043\000\044\000\060\000\
\084\000\061\000\009\000\062\000\045\000\064\000\085\000\010\000\
\086\000\091\000\046\000\087\000\011\000\032\000\032\000\032\000\
\032\000\032\000\032\000\088\000\095\000\032\000\032\000\032\000\
\032\000\098\000\097\000\099\000\032\000\010\000\032\000\032\000\
\034\000\034\000\034\000\034\000\034\000\034\000\102\000\026\000\
\034\000\034\000\100\000\027\000\040\000\041\000\042\000\034\000\
\034\000\034\000\034\000\035\000\035\000\035\000\035\000\035\000\
\035\000\043\000\044\000\035\000\035\000\045\000\009\000\010\000\
\065\000\000\000\035\000\000\000\035\000\035\000\054\000\055\000\
\056\000\057\000\058\000\059\000\000\000\000\000\041\000\042\000\
\043\000\044\000\041\000\042\000\043\000\044\000\041\000\042\000\
\043\000\044\000\000\000\053\000\000\000\063\000\041\000\042\000\
\043\000\044\000\000\000\000\000\000\000\066\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\016\000\000\000\016\000\016\000\016\000\
\016\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\000\000\000\000\000\000\000\000\000\016\000\016\000\
\000\000\000\000\000\000\016\000"

let yycheck = "\027\000\
\000\000\003\001\009\000\005\001\006\001\007\001\008\001\009\001\
\088\000\016\000\017\000\018\000\024\001\020\000\026\001\017\001\
\001\000\024\000\025\000\099\000\022\001\023\001\050\000\020\001\
\021\001\027\001\018\001\019\001\020\001\021\001\024\001\024\001\
\060\000\061\000\041\000\042\000\043\000\044\000\025\001\002\001\
\025\001\028\001\001\001\028\001\003\001\052\000\024\001\054\000\
\055\000\056\000\057\000\058\000\059\000\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\019\001\027\001\028\001\091\000\
\024\001\024\001\003\001\017\001\024\001\027\001\028\001\024\001\
\022\001\025\001\028\001\003\001\003\001\027\001\005\001\006\001\
\007\001\008\001\009\001\018\001\019\001\020\001\021\001\025\001\
\027\001\025\001\017\001\025\001\027\001\025\001\027\001\022\001\
\027\001\016\001\004\001\025\001\027\001\010\001\011\001\012\001\
\013\001\014\001\015\001\024\001\003\001\018\001\019\001\020\001\
\021\001\025\001\003\001\028\001\025\001\022\001\027\001\028\001\
\010\001\011\001\012\001\013\001\014\001\015\001\003\001\025\001\
\018\001\019\001\098\000\025\001\025\001\025\001\025\001\025\001\
\018\000\027\001\028\001\010\001\011\001\012\001\013\001\014\001\
\015\001\025\001\025\001\018\001\019\001\025\001\025\001\025\001\
\038\000\255\255\025\001\255\255\027\001\028\001\010\001\011\001\
\012\001\013\001\014\001\015\001\255\255\255\255\018\001\019\001\
\020\001\021\001\018\001\019\001\020\001\021\001\018\001\019\001\
\020\001\021\001\255\255\027\001\255\255\025\001\018\001\019\001\
\020\001\021\001\255\255\255\255\255\255\025\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\255\255\005\001\006\001\007\001\
\008\001\009\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\017\001\255\255\255\255\255\255\255\255\022\001\023\001\
\255\255\255\255\255\255\027\001"

let yynames_const = "\
  INT\000\
  IF\000\
  WHILE\000\
  SPRINT\000\
  IPRINT\000\
  SCAN\000\
  EQ\000\
  NEQ\000\
  GT\000\
  LT\000\
  GE\000\
  LE\000\
  ELSE\000\
  RETURN\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  LB\000\
  RB\000\
  LP\000\
  RP\000\
  ASSIGN\000\
  SEMI\000\
  COMMA\000\
  "

let yynames_block = "\
  NUM\000\
  STR\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 89 "parser.mly"
             (  _1  )
# 324 "parser.ml"
               : Absyn.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
             ( "int" )
# 330 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                   ( [] )
# 336 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 98 "parser.mly"
                ( match (_1,_2) with
                      (_::FunctionDec(_,_,_)::[],VarDec(_,_)::_) -> raise Semant_error
                    | _ -> _1@_2 )
# 346 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ids) in
    Obj.repr(
# 104 "parser.mly"
                     ( List.map (fun x -> VarDec (x,_1)) _2 )
# 354 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 105 "parser.mly"
                                    ( [FunctionDec(_2, _4, _6)] )
# 364 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "parser.mly"
                       ( _1@[_3] )
# 372 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
                       ( [_1]  )
# 379 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                        ( [] )
# 385 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fargs) in
    Obj.repr(
# 114 "parser.mly"
                        ( _1 )
# 392 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
                             ( _1@[(_4,_3)] )
# 401 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 118 "parser.mly"
                             ( [(_2,_1)] )
# 409 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 121 "parser.mly"
                   ( _1@[_2] )
# 417 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 122 "parser.mly"
                   ( [_1] )
# 424 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                              ( Assign (_1, _3) )
# 432 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 126 "parser.mly"
                              ( If (_3, _5, None) )
# 440 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 128 "parser.mly"
                              ( If (_3, _5, Some _7) )
# 449 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 130 "parser.mly"
                      ( While (_3, _5) )
# 457 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 132 "parser.mly"
                      ( CallProc ("sprint", [StrExp _3]) )
# 464 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                              ( CallProc ("iprint", [_3]) )
# 471 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 134 "parser.mly"
                           ( CallProc ("scan", [VarExp _3]) )
# 478 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 135 "parser.mly"
                           ( CallProc (_1, _3) )
# 486 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                           ( CallProc ("return", [_2]) )
# 493 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 137 "parser.mly"
             ( _1 )
# 500 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "parser.mly"
            ( NilStmt )
# 506 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
                           ( [] )
# 512 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aargs) in
    Obj.repr(
# 142 "parser.mly"
                           ( _1 )
# 519 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                          ( _1@[_3] )
# 527 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                         ( [_1] )
# 534 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 150 "parser.mly"
                         ( Block (_2, _3) )
# 542 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 153 "parser.mly"
           ( IntExp _1  )
# 549 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 154 "parser.mly"
          ( VarExp _1 )
# 556 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 155 "parser.mly"
                          ( CallFunc (_1, _3) )
# 564 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 156 "parser.mly"
                     ( CallFunc ("+", [_1; _3]) )
# 572 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 157 "parser.mly"
                     ( CallFunc ("-", [_1; _3]) )
# 580 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
                     ( CallFunc ("*", [_1; _3]) )
# 588 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "parser.mly"
                     ( CallFunc ("/", [_1; _3]) )
# 596 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "parser.mly"
                             ( CallFunc("!", [_2]) )
# 603 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 161 "parser.mly"
                   ( _2 )
# 610 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "parser.mly"
                     ( CallFunc ("==", [_1; _3]) )
# 618 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "parser.mly"
                     ( CallFunc ("!=", [_1; _3]) )
# 626 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 166 "parser.mly"
                     ( CallFunc (">", [_1; _3]) )
# 634 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 167 "parser.mly"
                     ( CallFunc ("<", [_1; _3]) )
# 642 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 168 "parser.mly"
                     ( CallFunc (">=", [_1; _3]) )
# 650 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 169 "parser.mly"
                     ( CallFunc ("<=", [_1; _3]) )
# 658 "parser.ml"
               : 'cond))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Absyn.stmt)
;;
