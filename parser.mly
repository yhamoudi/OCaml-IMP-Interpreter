%{
open Types 
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIV UMINUS
%token EQU NOT INF SUP AND OR
%token WHILE DO IF THEN ELSE SEQ AFF SKIP PRINT
%token FUN COMMA RETURN POINT
%token <string> FNAME
%token EOF 

%left SEQ   /* c1;c2;c3 ==  (c1;c2);c3 */
            /* while b do c1;c2 == (while b do c1);c2 , pareil pour if then else*/

%left THEN
%left IF
%left ELSE  /* if b1 then if b2 then c1 else c2 == if b1 then (if b2 then c1 else c2) */

%left WHILE
%left DO

%left OR
%left AND   /* and prioritaire sur or */
%right NOT

%left PLUS
%left MINUS
%left TIMES
%left DIV

%nonassoc FUN COMMA RETURN POINT UMINUS

%start main
%type <Types.fcom> main     /* on a un type fcom en entrée*/

%%

/* --- début des règles de grammaire --- */

main:
    fcom EOF    { $1 }  /* on veut reconnaître une commande au début */
;

fcom:   /* règles de grammaire pour le type fcom */
  | FUN FNAME LPAREN VAR COMMA VAR RPAREN EQU com POINT         { FunDef($2,$4,$6,$9) }
  | FUN FNAME LPAREN VAR COMMA VAR RPAREN EQU com POINT fcom    { SeqFunDef(($2,$4,$6,$9), $11) }
  | FUN FNAME LPAREN VAR RPAREN EQU com POINT                   { FunDef($2,$4,"HiddenVar",$7)}
  | FUN FNAME LPAREN VAR RPAREN EQU com POINT fcom              { SeqFunDef(($2,$4,"HiddenVar",$7), $9) }
  | FUN FNAME LPAREN RPAREN EQU com POINT                       { FunDef($2,"HiddenVar","HiddenVar",$6)}
  | FUN FNAME LPAREN RPAREN EQU com POINT fcom                  { SeqFunDef(($2,"HiddenVar","HiddenVar",$6),$8) }
  | com                                                         { EndFunDef($1) }
;

com:    /* règles de grammaire pour les commandes */
  | LPAREN com RPAREN            { $2 }
  | WHILE bexpr DO com           { While ($2,$4) }
  | IF bexpr THEN com ELSE com   { Ifte ($2,$4,$6) }
  | IF bexpr THEN com            { Ifte ($2,$4,Skip) }
  | com SEQ com                  { Seq ($1,$3) }
  | VAR AFF aexpr                { Aff ($1, $3) }
  | PRINT VAR                    { Print ($2) }
  | SKIP                         { Skip }
  | RETURN aexpr                 { Return ($2) }
;

aexpr:  /* règles de grammaire pour les expressions arithmetiques*/
  | INT                                     { Const $1 }
  | VAR                                     { Var $1 }
  | LPAREN aexpr RPAREN                     { $2 }         
  | aexpr PLUS aexpr                        { Add($1,$3) }
  | aexpr MINUS aexpr                       { Min($1,$3) }
  | aexpr TIMES aexpr                       { Mul($1,$3) }
  | aexpr DIV aexpr                         { Div($1,$3) }
  | MINUS aexpr %prec UMINUS                { Mul(Const (-1),$2) }
  | FNAME LPAREN aexpr COMMA aexpr RPAREN   { FunApp($1,$3,$5) }
  | FNAME LPAREN aexpr RPAREN               { FunApp($1,$3,Var "HiddenVar") }
  | FNAME LPAREN RPAREN                     { FunApp($1,Var "HiddenVar",Var "HiddenVar") }
;

bexpr:  /* règles de grammaire pour les expressions booleennes*/
  | BOOL                    { Boolean ($1) }
  | LPAREN bexpr RPAREN     { $2 }
  | aexpr EQU aexpr         { Equ($1,$3) }
  | NOT bexpr               { Not($2) }
  | aexpr INF aexpr         { Inf($1,$3) }
  | aexpr SUP aexpr         { Sup($1,$3) }
  | bexpr AND bexpr         { And($1,$3) }
  | bexpr OR bexpr          { Or($1,$3) }
;
