(* Header: da mesma forma que no modulo lexico, o header do módulo sintático 
   copia tudo da forma que está. *)
%{
  open Lexing
  (* Módulo contendo as definições dos nós da árvore sintática abstrata. *)
  open Ast
  open Sast
%}

(* Tokens lexicais da mini linguagem *)
%token <Lexing.position> APAR
%token <Lexing.position> FPAR
%token <Lexing.position> ACOL
%token <Lexing.position> FCOL
%token <Lexing.position> ATRIB
%token <Lexing.position> IF
%token <Lexing.position> WHILE
%token <Lexing.position> MAIS
%token <Lexing.position> PUBLIC
%token <Lexing.position> CLASS
%token <Lexing.position> STATIC
%token <Lexing.position> VOID
%token <Lexing.position> ACHAVE
%token <Lexing.position> FCHAVE
%token <Lexing.position> INT
%token <Lexing.position> PTV
%token <Lexing.position> PTO
%token <Lexing.position> MENOS
%token <Lexing.position> ELSE
%token <Lexing.position> IGUAL
%token <Lexing.position> DIFER
%token <Lexing.position> MAIOR
%token <Lexing.position> MENOR
%token <Lexing.position> MAIORIGUAL
%token <Lexing.position> MENORIGUAL
%token <Lexing.position> ELOG
%token <Lexing.position> OULOG
%token <Lexing.position> NOT
%token <Lexing.position> STRING
%token <float * Lexing.position> LITFLOAT
%token <Lexing.position> VEZES
%token <Lexing.position> DIV
%token <Lexing.position> VIRG
%token <Lexing.position> FLOAT
%token <Lexing.position> READFLOAT
%token <Lexing.position> READINT
%token <Lexing.position> READCHAR
%token <Lexing.position> READSTRING
%token <Lexing.position> MAISMAIS
%token <Lexing.position> MENOSMENOS
%token <Lexing.position> DPTOS
%token <Lexing.position> SWITCH
%token <Lexing.position> DEFAULT
%token <Lexing.position> CASE
%token <Lexing.position> BREAK
%token <Lexing.position> CHAR
%token <Lexing.position> FOR
%token <Lexing.position> MAISIGUAL
%token <Lexing.position> MENOSIGUAL
%token <Lexing.position> VEZESIGUAL
%token <Lexing.position> BOOLEAN
%token <Lexing.position> DIVIGUAL
%token <Lexing.position> ARGV
%token <Lexing.position> MOD
%token <Lexing.position> RETURN
%token <bool * Lexing.position> LITBOOL
%token <int * Lexing.position> LITINT
%token <string * Lexing.position> LITSTRING
%token <char * Lexing.position> LITCHAR
%token <string * Lexing.position> ID
%token <Lexing.position> PRINT
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
%start <Sast.expressao Ast.programa> programa
%%

programa : PUBLIC CLASS ID ACHAVE cs=funcoes FCHAVE EOF { Programa cs }
         ;

funcoes :                     { [] }
        | f=funcao fs=funcoes { f :: fs }
        ;

funcao : PUBLIC STATIC t=tipo nome=ID APAR params=parametros FPAR ACHAVE
           ds=declaracao*
           cs=comandos
         FCHAVE { DecFun {fn_nome=nome; fn_tiporet=t; fn_formais=params; fn_locais=(List.flatten ds); fn_corpo=cs} }
       ;

declaracao : t=tipo; ids=separated_nonempty_list(VIRG, ID); PTV { List.map (fun id -> DecVar (id, t)) ids }
           ;

parametros :                                { [] }
           | p=parametro                    { [p] }
           | p=parametro VIRG ps=parametros { p :: ps }
           ;

parametro : t=tipo x=ID { (x, t) }
          ;

(* Lista de comandos. *)
comandos :                       { [] }
         | c=comando cs=comandos { c :: cs }
         ;

(* Comandos da mini linguagem. *)
comando : c=comando_s PTV { c }
        | c=comando_c     { c }
        ;

(* Comandos compostos: comandos que possuem inicializacao e corpo, mas não terminam com ";" *)
comando_c : c=cmd_while  { c }
          | c=cmd_for    { c }
          | c=cmd_if     { c }
          | c=cmd_switch { c }
          ;

cmd_while : WHILE APAR e=expr FPAR ACHAVE cs=comandos FCHAVE { CmdWhile (e, cs) }
          ;

cmd_for : FOR APAR ci=comando_s PTV e=expr PTV ca=comando_s FPAR ACHAVE cs=comandos FCHAVE { CmdFor (ci, e, ca, cs) }
        ;

cmd_if : IF APAR e=expr FPAR ACHAVE cs=comandos FCHAVE pe=parte_else { CmdIf (e, cs, pe) }
       ;

cmd_switch : SWITCH APAR e=expr FPAR ACHAVE cs=cases d=default FCHAVE { CmdSwitch (e, cs, d) }
           ;

cases :                 { [] }
      | c=case cs=cases { c :: cs }
      ;

case : CASE e=expr DPTOS cs=comandos BREAK PTV { Case (e, cs) }
     ;

default:                                     { None }
       | DEFAULT DPTOS cs=comandos BREAK PTV { Some (Default cs) }
       ;

parte_else :                                { None }
           | ELSE ACHAVE cs=comandos FCHAVE { Some cs }
           | ELSE c=cmd_if                  { Some [c] }
           ;

(* Comandos simples: comandos que devem terminar com ";" *)
comando_s : c=cmd_atrib                     { c }
          | e=expr ATRIB READINT            { CmdReadInt e }
          | e=expr ATRIB READFLOAT          { CmdReadFloat e }
          | e=expr ATRIB READSTRING         { CmdReadString e }
          | e=expr ATRIB READCHAR           { CmdReadChar e }
          | PRINT APAR args=argumentos FPAR { CmdPrint args }
          | c=cmd_return                    { c }
          | e=exp_fun                       { CmdFun e }
          ;

argumentos :                             { [] }
           | e=expr                      { [e] }
           | e=expr VIRG args=argumentos { e :: args }
           ;

(* Comandos de atribuição. *)
cmd_atrib : ee=expr ATRIB ed=expr          { CmdAtrib (ee, ed) }
          | ee=expr pos=MAISIGUAL ed=expr  { CmdAtrib (ee, ExpOp ((Soma, pos), ee, ed)) }
          | ee=expr pos=MENOSIGUAL ed=expr { CmdAtrib (ee, ExpOp ((Sub, pos), ee, ed)) }
          | ee=expr pos=VEZESIGUAL ed=expr { CmdAtrib (ee, ExpOp ((Mult, pos), ee, ed)) }
          | ee=expr pos=DIVIGUAL ed=expr   { CmdAtrib (ee, ExpOp ((Div, pos), ee, ed)) }
          (*| ee=expr pos=MAISMAIS           { CmdAtrib (ee, ExpOp ((Soma, pos), ee, ExpInt 1)) }*)
          (*| ee=expr pos=MENOSMENOS         { CmdAtrib (ee, ExpOp ((Sub, pos), ee, ExpInt 1)) }*)
          ;

(* Comando de retorno. *)
cmd_return : RETURN e=expr { CmdReturn (Some e) }
           | RETURN        { CmdReturn None }
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
     | ef=exp_fun                   { ef }
     ;

(* Uma chamada de função. *)
exp_fun : x=ID APAR args=argumentos FPAR { ExpFun (x, args) }
        ;

(* Operadores da mini linguagem. *)
%inline oper : pos=MAIS       { (Soma, pos) }
             | pos=MENOS      { (Sub, pos) }
             | pos=IGUAL      { (Igual, pos) }
             | pos=DIFER      { (Difer, pos) }
             | pos=MAIOR      { (Maior, pos) }
             | pos=MENOR      { (Menor, pos) }
             | pos=MAIORIGUAL { (MaiorIgual, pos) }
             | pos=MENORIGUAL { (MenorIgual, pos) }
             | pos=ELOG       { (E, pos) }
             | pos=OULOG      { (Ou, pos) }
             | pos=NOT        { (Not, pos) }
             | pos=MOD        { (Mod, pos) }
             | pos=DIV        { (Div, pos) }
             | pos=VEZES      { (Mult, pos) }
             ;

(* Variaveis *)
variavel : x=ID {VarSimples x }
         ;

(* Tipos primitivos da mini linguagem. *)
tipo : BOOLEAN { Bool }
     | INT     { Int }
     | FLOAT   { Float }
     | CHAR    { Char }
     | STRING  { String }
     | VOID    { Void }
     ;

