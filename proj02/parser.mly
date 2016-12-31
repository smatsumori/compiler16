%{

open Printf;;
open Absyn;;

exception Semant_error;;
let parse_error s =
    raise Parsing.Parse_error;;
%}

/* Declarations */
/* File parser.mly */

%token <int> NUM
%token <string> STR ID
%token INT IF WHILE SPRINT IPRINT SCAN EQ NEQ GT LT GE LE ELSE RETURN
%token ADD SUB MUL DIV LB RB LP RP ASSIGN SEMI COMMA 
%type <Absyn.stmt> prog


%nonassoc GT LT EQ NEQ GE LE
%left ADD SUB         /* lowest precedence */
%left MUL DIV         /* medium precedence */
%nonassoc UMINUS      /* highest precedence */


%start prog           /* the entry point */

%%

/* Rules */

prog : stmt  {  $1  }
     ;

ty   : INT   { "int" }
     ;

/* sequential dec */
decs : /* empty */ { [] }
     | decs dec { match ($1,$2) with
                      (_::FunctionDec(_,_,_)::[],VarDec(_,_)::_) -> raise Semant_error
                    | _ -> $1@$2 }
     ;
/* declaration of function and variables */
/* block : function body */
dec  : ty ids SEMI   { List.map (fun x -> VarDec (x,$1)) $2 }
     | ty ID LP fargs_opt RP block  { [FunctionDec($2, $4, $6)] }
     ; 

ids  : ids COMMA ID    { $1@[$3] }
     | ID              { [$1]  }
     ;

/* if there is function args or not */
fargs_opt : /* empty */ { [] }
     | fargs            { $1 }
     ;
     
fargs: fargs COMMA ty ID     { $1@[($4,$3)] }
     | ty ID                 { [($2,$1)] }
     ;

stmts: stmts stmt  { $1@[$2] }
     | stmt        { [$1] }
     ;

stmt : ID ASSIGN expr SEMI    { Assign ($1, $3) }
     | IF LP cond RP stmt     { If ($3, $5, None) }  /* with no else */
     | IF LP cond RP stmt ELSE stmt     /* with else */
                              { If ($3, $5, Some $7) }
     | WHILE LP cond RP stmt 
                      { While ($3, $5) }
     | SPRINT LP STR RP SEMI
                      { CallProc ("sprint", [StrExp $3]) }
     | IPRINT LP expr RP SEMI { CallProc ("iprint", [$3]) }
     | SCAN LP ID RP SEMI  { CallProc ("scan", [VarExp $3]) }
     | ID LP aargs_opt RP  { CallProc ($1, $3) }    /* function call */
     | RETURN expr SEMI    { CallProc ("return", [$2]) }    /* end of a function */
     | block { $1 }
     | SEMI { NilStmt } /* empty statement */
     | error SEMI { DummyStmt }
     ;

aargs_opt: /* empty */     { [] }
        | aargs            { $1 }
        ;

aargs :  aargs COMMA expr { $1@[$3] }
      | expr             { [$1] }
      ;

/* declare var first */
block: LB decs stmts RB  { Block ($2, $3) }
     ;

expr : NUM { IntExp $1  }
     | ID { VarExp $1 }
     | ID LP aargs_opt RP { CallFunc ($1, $3) }
     | expr ADD expr { CallFunc ("+", [$1; $3]) }
     | expr SUB expr { CallFunc ("-", [$1; $3]) }
     | expr MUL expr { CallFunc ("*", [$1; $3]) }
     | expr DIV expr { CallFunc ("/", [$1; $3]) }
     | SUB expr %prec UMINUS { CallFunc("!", [$2]) } /* inv sign */
     | LP expr RP  { $2 }
     | LP error RP { DummyExp }
     ;

cond : expr EQ expr  { CallFunc ("==", [$1; $3]) }
     | expr NEQ expr { CallFunc ("!=", [$1; $3]) }
     | expr GT expr  { CallFunc (">", [$1; $3]) }
     | expr LT expr  { CallFunc ("<", [$1; $3]) }
     | expr GE expr  { CallFunc (">=", [$1; $3]) }
     | expr LE expr  { CallFunc ("<=", [$1; $3]) }
     ;
%%
