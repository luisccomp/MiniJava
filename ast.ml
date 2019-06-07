open Lexing

(* Tipo identificador. *)
type ident = string
type 'a pos = 'a * Lexing.position (* Tipo e posição no arquivo fonte. *)


type 'expr programa = Programa of 'expr funcoes

and 'expr comandos = ('expr comando) list

and declaracao = DecVar of (ident pos) * tipo

and declaracoes = declaracao list

and 'expr funcoes = ('expr funcao) list

and 'expr funcao = DecFun of ('expr decfn)

(* Declaracao de função. *)
and 'expr decfn = {
  fn_nome:    ident pos;
  fn_tiporet: tipo;
  fn_formais: (ident pos * tipo) list;
  fn_locais: declaracoes;
  fn_corpo: 'expr comandos;
}

(* Comandos da mini linguagem. *)
and 'expr comando = CmdAtrib of 'expr * 'expr (*OK*)
            | CmdReadInt of 'expr (*OK*)
            | CmdReadFloat of 'expr (*OK*)
            | CmdReadString of 'expr (*OK*)
            | CmdReadChar of 'expr (*OK*)
            | CmdWhile of 'expr * ('expr comandos) (*OK*)
            | CmdFor of ('expr comando) * 'expr * ('expr comando) * ('expr comandos) (*OK*)
            | CmdIf of 'expr * ('expr comandos) * ('expr comandos) option (*OK*)
            | CmdPrint of 'expr expressoes
            | CmdReturn of 'expr option (*OK*)
            | CmdSwitch of 'expr * ('expr cases) * ('expr default) option
            | CmdFun of 'expr (*OK*)

and 'expr cases = ('expr case) list

and 'expr case = Case of 'expr * ('expr comandos)

and 'expr default = Default of 'expr comandos

and 'expr expressoes = 'expr list

(* Tipo de expressões da mini linguagem. *)
(*and expressao = ExpVar of variavel
              | ExpInt of int
              | ExpFloat of float
              | ExpBool of bool
              | ExpString of string
              | ExpChar of char
              | ExpOp of oper * expressao * expressao
              | ExpUn of oper * expressao
              | ExpFun of ident * expressoes*)

(* Operadores da mini linguagem *)
and oper = Soma
         | Sub
         | Div
         | Mult
         | E
         | Ou
         | Mod
         | Maior
         | Menor
         | MaiorIgual
         | MenorIgual
         | Igual
         | Difer
         | Not
         | UMenos

(* Tipo de variaveis da mini linguagem. *)
and 'expr variavel = VarSimples of ident pos
and 'expr variaveis = ('expr variavel) list

(* Tipos primitivos da mini linguagem. *)
and tipo = Int
         | Float
         | Char
         | String
         | Void
         | Bool

