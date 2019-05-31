{
  (* Importando o módulo sintático. Os tokens lexicais serão declarados nesse módulo. *)
  open Sintatico
  open Lexing
  open Printf


  (* Exceptions para erro léxico *)
  exception Erro of string


  (* Incrmenta o contador de linha do analisador léxico para controlar qual
     linha ele está analisando no presente momento. *)
  let incr_num_linha lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_cnum = pos.pos_bol;
    }


  (* Gera uma mensagem de erro de caractere desconhecido. *)
  let msg_erro lexbuf c =
    let pos = lexbuf.lex_curr_p in
    let lin = pos.pos_lnum
    and col = pos.pos_cnum - pos.pos_bol - 1 in
    sprintf "%d-%d: caractere desconhecido '%c'" lin col c


  (* Cria uma exceção baseada em uma mensagem de erro genérica. *)
  let erro lin col msg =
    let mensagem = sprintf "%d-%d: %s" lin col msg in
    mensagem    


  (* Retorna a posição atual do analisador léxico. *)  
  let pos_atual lexbuf = lexbuf.lex_start_p
}

(* Abreviações: se as regras forem muito extensas, uma boa prática é criar
   abreviações para aumentar a legibilidade do código fonte do analisador
   léxico. *)
let digito = ['0'-'9']
let inteiro = digito+

let letra = ['a' - 'z' 'A' - 'Z']
let identificador = letra (letra | digito | '_')*

let numfloat = digito+ '.' digito* | digito* '.' digito+

let booleano = "true" | "false"

let caractere = '\'' letra '\''

let brancos = [' ' '\t']+
let novalinha = '\r' | '\n' | "\r\n"

let comentario = "//" [^ '\r' '\n']*

(* Regras lexicais: aqui estão as regras de produção dos tokens lexicais da
   mini linguagem. *)
rule token = parse
  brancos                               { token lexbuf }                        (* Descarta o token *)
| novalinha                             { incr_num_linha lexbuf; token lexbuf } (* Incrementa o numero de linha e descarta o token *)
| comentario                            { token lexbuf }                        (* Descarta o token *)
| "Scanner s = new Scanner(System.in);" { token lexbuf }                        (* Descarta o token *)
| "import java.util.Scanner;"           { token lexbuf }                        (* Descarta o token *)
| "/*"                                  { let pos = lexbuf.lex_curr_p in 
                                          let lin = pos.pos_lnum
                                          and col = pos.pos_cnum - pos.pos_bol - 1 in
                                          comentario_bloco lin col 0 lexbuf }
| '('                                   { APAR (pos_atual lexbuf) }
| ')'                                   { FPAR (pos_atual lexbuf) }
| '['                                   { ACOL (pos_atual lexbuf) }
| ']'                                   { FCOL (pos_atual lexbuf) }
| "String[] args"                       { token lexbuf }
| "+="                                  { MAISIGUAL (pos_atual lexbuf) }
| "-="                                  { MENOSIGUAL (pos_atual lexbuf) }
| "*="                                  { VEZESIGUAL (pos_atual lexbuf) }
| "/="                                  { DIVIGUAL (pos_atual lexbuf) }
| "++"                                  { MAISMAIS (pos_atual lexbuf) }
| "--"                                  { MENOSMENOS (pos_atual lexbuf) }
| '+'                                   { MAIS (pos_atual lexbuf) }
| '='                                   { ATRIB (pos_atual lexbuf) }
| "public"                              { PUBLIC (pos_atual lexbuf) }
| "%"                                   { MOD (pos_atual lexbuf) }
| "char"                                { CHAR (pos_atual lexbuf) }
| "boolean"                             { BOOLEAN (pos_atual lexbuf) }
| "return"                              { RETURN (pos_atual lexbuf) }
| "default"                             { DEFAULT (pos_atual lexbuf) }
| "class"                               { CLASS (pos_atual lexbuf) }
| "static"                              { STATIC (pos_atual lexbuf) }
| "void"                                { VOID (pos_atual lexbuf) }
| "System.out.printf"                   { PRINT (pos_atual lexbuf) } (* Instrução de impressão na tela *)
| "switch"                              { SWITCH (pos_atual lexbuf) }
| "case"                                { CASE (pos_atual lexbuf) }
| "break"                               { BREAK (pos_atual lexbuf) }
| '{'                                   { ACHAVE (pos_atual lexbuf) }
| '}'                                   { FCHAVE (pos_atual lexbuf) }
| "int"                                 { INT (pos_atual lexbuf) }
| "float"                               { FLOAT (pos_atual lexbuf) }
| "Float.parseFloat(s.nextLine())"      { READFLOAT (pos_atual lexbuf) }
| "Integer.parseInt(s.nextLine())"      { READINT (pos_atual lexbuf) }
| "s.nextLine().charAt(0)"              { READCHAR (pos_atual lexbuf) }
| "s.nextLine()"                        { READSTRING (pos_atual lexbuf) }
| ';'                                   { PTV (pos_atual lexbuf) }
| ':'                                   { DPTOS (pos_atual lexbuf) }
| '.'                                   { PTO (pos_atual lexbuf) }
| '-'                                   { MENOS (pos_atual lexbuf) }
| "=="                                  { IGUAL (pos_atual lexbuf) }
| "!="                                  { DIFER (pos_atual lexbuf) }
| '>'                                   { MAIOR (pos_atual lexbuf) }
| '<'                                   { MENOR (pos_atual lexbuf) }
| ">="                                  { MAIORIGUAL (pos_atual lexbuf) }
| "<="                                  { MENORIGUAL (pos_atual lexbuf) }
| "&&"                                  { ELOG (pos_atual lexbuf) }
| "||"                                  { OULOG (pos_atual lexbuf) }
| '!'                                   { NOT (pos_atual lexbuf) }
| "String"                              { STRING (pos_atual lexbuf) }
| '*'                                   { VEZES (pos_atual lexbuf) }
| '/'                                   { DIV (pos_atual lexbuf) }
| ','                                   { VIRG (pos_atual lexbuf) }
| booleano as b                         { let valor_booleano = bool_of_string b in
                                          LITBOOL (valor_booleano, pos_atual lexbuf) }
| caractere as chr                      { LITCHAR (chr.[1], pos_atual lexbuf) }
| numfloat as num                       { let numero = float_of_string num in
                                          LITFLOAT (numero, posatual_lexbuf) }
| inteiro as num                        { let numero = int_of_string num in
                                          LITINT (numero, pos_atual lexbuf) }
| "if"                                  { IF (pos_atual lexbuf) }
| "else"                                { ELSE (pos_atual lexbuf) }
| "while"                               { WHILE (pos_atual lexbuf) }
| "for"                                 { FOR (pos_atual lexbuf) }
| identificador as id                   { ID (id, pos_atual lexbuf) }
| '"'                                   { let pos = lexbuf.lex_curr_p in
                                          let lin = pos.pos_lnum
                                          and col = pos.pos_cnum - pos.pos_bol - 1 in
                                          let buffer = Buffer.create 1 in
                                          let str = leia_string lin col buffer lexbuf in
                                          LITSTRING (str, pos_atual lexbuf) }
(*| _ as c                                { failwith (msg_erro lexbuf c) }*)
| _ as c                                { raise (Erro (msg_erro lexbuf c)) }
| eof                                   { EOF }
and leia_string lin col buffer = parse
  '"'                                   { Buffer.contents buffer }
| "\\t"                                 { Buffer.add_char buffer '\t';
                                          leia_string lin col buffer lexbuf }
| "\\n"                                 { Buffer.add_char buffer '\n';
                                          leia_string lin col buffer lexbuf }
| '\\' '"'                              { Buffer.add_char buffer '"';
                                          leia_string lin col buffer lexbuf }
| '\\' '\\'                             { Buffer.add_char buffer '\\';
                                          leia_string lin col buffer lexbuf }
| _ as c                                { Buffer.add_char buffer c;
                                          leia_string lin col buffer lexbuf }
(*| eof                                   { erro lin col "A string nao foi fechada" }*)
| eof                                   { raise (Erro (erro lin col "A string nao foi fechada")) }
and comentario_bloco lin col n = parse
  "*/"                                  { if n=0 then token lexbuf
                                          else comentario_bloco lin col (n-1) lexbuf }
| "/*"                                  { let pos = lexbuf.lex_curr_p in
                                          let lin = pos.pos_lnum
                                          and col = pos.pos_cnum - pos.pos_bol - 1 in
                                          comentario_bloco lin col (n+1) lexbuf }
| novalinha                             { incr_num_linha lexbuf;
                                          comentario_bloco lin col n lexbuf }
| _                                     { comentario_bloco lin col n lexbuf }
(*| eof                                   { erro lin col "Comentario bloco nao fechado" }*)
| eof { raise (Erro (erro lin col "A string nao foi fechada")) }
