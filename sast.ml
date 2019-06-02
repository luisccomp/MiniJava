open Ast

type expressao = ExpVar of (expressao variavel)
               | ExpInt of int pos
               | ExpFloat of float pos
               | ExpBool of bool pos
               | ExpString of string pos
               | ExpChar of char pos
               | ExpOp of (oper pos) * expressao * expressao
               | ExpUn of (oper pos) * expressao
               | ExpFun of (ident pos) * expressao expressoes

