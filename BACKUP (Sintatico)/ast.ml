
(* Tipo identificador. *)
type ident = string


type programa = Programa of funcoes

and comandos = comando list

and declaracao = DecVar of ident * tipo

and declaracoes = declaracao list

and funcoes = funcao list

and funcao = DecFun of decfn

(* Declaracao de função. *)
and decfn = {
  fn_nome:    ident;
  fn_tiporet: tipo;
  fn_formais: (ident * tipo) list;
  fn_locais: declaracoes;
  fn_corpo: comandos;
}

(* Comandos da mini linguagem. *)
and comando = CmdAtrib of expressao * expressao
            | CmdReadInt of expressao
            | CmdReadFloat of expressao
            | CmdReadString of expressao
            | CmdReadChar of expressao
            | CmdWhile of expressao * comandos
            | CmdFor of comando * expressao * comando * comandos
            | CmdIf of expressao * comandos * comandos option
            | CmdPrint of expressoes
            | CmdReturn of expressao option
            | CmdSwitch of expressao * cases * default option
            | CmdFun of expressao

and cases = case list

and case = Case of expressao * comandos

and default = Default of comandos

and expressoes = expressao list

(* Tipo de expressões da mini linguagem. *)
and expressao = ExpVar of variavel
              | ExpInt of int
              | ExpFloat of float
              | ExpBool of bool
              | ExpString of string
              | ExpChar of char
              | ExpOp of oper * expressao * expressao
              | ExpUn of oper * expressao
              | ExpFun of ident * expressoes

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

(* Tipo de variaveis da mini linguagem. *)
and variavel = VarSimples of ident

(* Tipos primitivos da mini linguagem. *)
and tipo = Int
         | Float
         | Char
         | String
         | Void
         | Bool

