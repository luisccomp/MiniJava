(* Header: da mesma forma que no modulo lexico, o header do módulo sintático 
   copia tudo da forma que está. *)
%{
  (* Módulo contendo as definições dos nós da árvore sintática abstrata. *)
  open Ast
%}

(* Tokens lexicais da mini linguagem *)
%token APAR
%token FPAR
%token ACOL
%token FCOL
%token ATRIB
%token IF
%token WHILE
%token MAIS
%token PUBLIC
%token CLASS
%token STATIC
%token VOID
%token ACHAVE
%token FCHAVE
%token INT
%token PTV
%token PTO
%token MENOS
%token ELSE
%token IGUAL
%token DIFER
%token MAIOR
%token MENOR
%token MAIORIGUAL
%token MENORIGUAL
%token ELOG
%token OULOG
%token NOT
%token STRING
%token <float> LITFLOAT
%token VEZES
%token DIV
%token VIRG
%token FLOAT
%token READFLOAT
%token READINT
%token READCHAR
%token READSTRING
%token MAISMAIS
%token MENOSMENOS
%token DPTOS
%token SWITCH
%token DEFAULT
%token CASE
%token BREAK
%token CHAR
%token FOR
%token MAISIGUAL
%token MENOSIGUAL
%token VEZESIGUAL
%token BOOLEAN
%token DIVIGUAL
%token ARGV
%token MOD
%token RETURN
%token <bool> LITBOOL
%token <int> LITINT
%token <string> LITSTRING
%token <char> LITCHAR
%token <string> ID
%token PRINT
%token EOF

(* Precedência de tokens *)
%left OULOG
%left ELOG
%left IGUAL DIFER
%left MAIOR MENOR MAIORIGUAL MENORIGUAL
%left MAIS MENOS
%left VEZES DIV MOD

%right NOT
%right UMENOS

(* Simbolo inicial da gramatica. *)
%start <Ast.programa> programa
%%

programa : cs=comandos EOF { Programa cs }
         ;

(* Lista de comandos. *)
comandos :                       { [] }
         | c=comando cs=comandos { c :: cs }
         ;

(* Comandos da mini linguagem. *)
comando : c=comando_s PTV { c }
        ;

(* Comandos simples: comandos que devem terminar com ";" *)
comando_s : c=cmd_atrib             { c }
          | e=expr ATRIB READINT    { CmdReadInt e }
          | e=expr ATRIB READFLOAT  { CmdReadFloat e }
          | e=expr ATRIB READSTRING { CmdReadString e }
          | e=expr ATRIB READCHAR   { CmdReadChar e }
          ;

(* Comandos de atribuição. *)
cmd_atrib : ee=expr ATRIB ed=expr      { CmdAtrib (ee, ed) }
          | ee=expr MAISIGUAL ed=expr  { CmdAtrib (ee, ExpOp (Soma, ee, ed)) }
          | ee=expr MENOSIGUAL ed=expr { CmdAtrib (ee, ExpOp (Sub, ee, ed)) }
          | ee=expr VEZESIGUAL ed=expr { CmdAtrib (ee, ExpOp (Mult, ee, ed)) }
          | ee=expr DIVIGUAL ed=expr   { CmdAtrib (ee, ExpOp (Div, ee, ed)) }
          | ee=expr MAISMAIS           { CmdAtrib (ee, ExpOp (Soma, ee, ExpInt 1)) }
          | ee=expr MENOSMENOS         { CmdAtrib (ee, ExpOp (Sub, ee, ExpInt 1)) }
          ;

(* Expressões da mini linguagem. *)
expr : APAR e=expr FPAR             { e }
     | v=variavel                   { ExpVar v }
     | i=LITINT                     { ExpInt i }
     | f=LITFLOAT                   { ExpFloat f }
     | b=LITBOOL                    { ExpBool b }
     | s=LITSTRING                  { ExpString s }
     | c=LITCHAR                    { ExpChar c }
     | ee=expr op=oper ed=expr      { ExpOp (op, ee, ed) }
     | op=oper ed=expr %prec UMENOS { ExpUn (op, ed) }
     ;

%inline oper : MAIS       { Soma }
             | MENOS      { Sub }
             | IGUAL      { Igual }
             | DIFER      { Difer }
             | MAIOR      { Maior }
             | MENOR      { Menor }
             | MAIORIGUAL { MaiorIgual }
             | MENORIGUAL { MenorIgual }
             | ELOG       { E }
             | OULOG      { Ou }
             | NOT        { Not }
             | MOD        { Mod }
             | DIV        { Div }
             | VEZES      { Mult }
             ;

(* Variaveis *)
variavel : x=ID {VarSimples x }
         ;



































