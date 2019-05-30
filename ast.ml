
(* Tipo identificador. *)
type ident = string


type programa = Programa of comandos

and comandos = comando list

(* Comandos da mini linguagem. *)
and comando = CmdAtrib of expressao * expressao
            | CmdReadInt of expressao
            | CmdReadFloat of expressao
            | CmdReadString of expressao
            | CmdReadChar of expressao

(* Tipo de expressões da mini linguagem. *)
and expressao = ExpVar of variavel
              | ExpInt of int
              | ExpFloat of float
              | ExpBool of bool
              | ExpString of string
              | ExpChar of char
              | ExpOp of oper * expressao * expressao
              | ExpUn of oper * expressao

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

