(* Nó da arvore sintatica tipada. Esse nó acompanha o tipo dele. *)
open Ast


type expressao = ExpVar of (expressao variavel) * tipo
               | ExpInt of int * tipo
               | ExpFloat of float * tipo
               | ExpBool of bool * tipo
               | ExpString of string * tipo
               | ExpChar of char * tipo
               | ExpOp of (oper * tipo) * (expressao * tipo) * (expressao * tipo)
               | ExpUn of (oper * tipo) * expressao
               | ExpFun of ident * (expressao * tipo) list

