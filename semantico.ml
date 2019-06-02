(* Criando "aliases" para os módulos. *)
module Amb = Ambiente
module A = Ast
module S = Sast
module T = Tast


(* Retorna a posição do token no arquivo fonte. *)
let rec posicao exp = let open S in 
    match exp with
      ExpVar v ->
         (match v with
            A.VarSimples (_, pos) -> pos
          | _ -> failwith "Tipo variavel desconhecido!"
         )
    | ExpInt (_, pos) -> pos
    | ExpFloat (_, pos) -> pos
    | ExpChar (_, pos) -> pos
    | ExpString (_, pos) -> pos
    | ExpBool (_, pos) -> pos
    | ExpOp ((_, pos), _, _) -> pos
    | ExpUn ((_, pos), _) -> pos
    | ExpFun ((_, pos), _) -> pos


(* Classes dos operadores da mini linguagem *)
type classe_op = Aritimetico
    | Relacional
    | Logico


(* Dado um operador, retorna a classe a qual ele pertence. *)
let classifica op = let open A in
    match op with
      Ou
    | E -> Logico
    | Menor
    | Maior
    | MenorIgual
    | MaiorIgual
    | Igual
    | Difer -> Relacional
    | Soma
    | Sub
    | Mult
    | Div
    | Mod -> Aritimetico


(* Retorna uma mensagem de erro e o local no arquivo fonte onde ela foi lançada. *)
let msg_erro_pos pos msg =
    let open Lexing in
    let lin = pos.pos_lnum
    and col = pos.pos_cnum - pos.pos_bol - 1 in
    Printf.sprintf "Semantico -> linha %d, coluna %d: %s" lin col msg


let msg_erro nome msg =
    let pos = snd nome in
    msg_erro_pos pos msg


(* Retorna o nome de um tipo da mini linguagem. *)
let nome_tipo t = let open A in
    match t with
      Int -> "int"
    | Float -> "float"
    | Char -> "char"
    | String -> "string"
    | Void -> "void"
    | Bool -> "bool"


(* Verifica se um tipo inferido e um tipo declarado são iguais. *)
let mesmo_tipo pos msg tinf tdec =
    if tinf <> tdec then
        let msg = Printf.sprintf msg (nome_tipo tinf) (nome_tipo tdec) in
        failwith (msg_erro_pos pos msg)


(* Infere o tipo de uma expressão. *)
let rec infere_exp amb exp =
    match exp with
      S.ExpInt i -> (T.ExpInt (fst i, A.Int), A.Int)
    | S.ExpFloat f -> (T.ExpFloat (fst f, A.Float), A.Float)
    | S.ExpChar c -> (T.ExpChar (fst c, A.Char), A.Char)
    | S.ExpString s -> (T.ExpString (fst s, A.String), A.String)
    | S.ExpBool b -> (T.ExpChar (fst b, A.Bool), A.Bool)
    | S.ExpVar v ->
       (match v with
          A.VarSimples nome ->
            (* Tenta encontrar a definição da variável no escopo local, se não        *)
            (* encontrar, tenta novamente no escopo que engloba o atual. Prossegue-se *)
            (* assim até encontrar a definição em algum escopo englobante ou até      *)
            (* encontrar o escopo global. Se em algum lugar for encontrado,           *)
            (* devolve-se a definição. Em caso contrário devolve uma exceção.         *)
            let id = fst nome in
               (try
                   (match (Amb.busca amb id) with
                      Amb.EntVar tipo -> (T.ExpVar (A.VarSimples nome, tipo), tipo)
                    | Amb.EntFun _ ->
                          let msg = "nome de funcao usado como variavel: " ^ id in
                          failwith (msg_erro nome msg))
                with Not_found ->
                    let msg = "A variavel " ^ id ^ " nao foi declarada." in
                    failwith (msg_erro nome msg))
        | _ -> failwith "infere_exp nao implementado")
    | S.ExpOp (op, esq, dir) ->
        let (esq, tesq) = infere_exp amb esq    (* Inferindo o tipo da expressão da esquerda *)
        and (dir, tdir) = infere_exp amb dir in (* Inferindo o tipo de expressao da direita  *)

        (* Verifica os tipos para operador aritimetico. *)
        let verifica_aritimetico () =
           (match tesq with
              A.Int ->
                  let _ = mesmo_tipo (snd op)
                                     "O operando da esquerda eh do tipo %s mas o da direita eh do tipo %s"
                                     tesq
                                     tdir in
                  tesq (* O tipo da expressão aritimética como um todo. *)
            | A.Float ->
                  let _ = mesmo_tipo (snd op)
                                     "O operando da esquerda eh do tipo %s mas o da direita eh do tipo %s"
                                     tesq
                                     tdir in
                  tesq
            | t -> let msg = "Um operador aritimetico nao pode ser usado com o tipo " ^ (nome_tipo t) in
                   failwith (msg_erro_pos (snd op) msg))
        
        (* Verifica os tipos para um operador relacional. *)
        and verifica_relacional () =
           (match tesq with
              A.Int
            | A.Float
            | A.Char ->
                let _ = mesmo_tipo (snd op)
                                   "O operando da esquerda eh do tipo %s mas o da direita eh do tipo %s"
                                   tesq
                                   tdir in
                A.Bool (* O tipo de uma expressao relacional eh sempre booleano. *)
            | t -> let msg = "Um operador relacional nao pode ser usado com o tipo " ^ (nome_tipo t) in
                   failwith (msg_erro_pos (snd op) msg))

        (* Verifica os tipos para um operador loeigo. *)
        and verifica_logico () =
           (match tesq with
              A.Bool ->
                let _ = mesmo_tipo (snd op)
                                   "O operando da esquerda eh do tipo %s mas o da direita eh do tipo %s"
                                   tesq
                                   tdir in
                A.Bool (* O tipo de uma expressao logica eh sempre booleano. *)
            | t -> let msg = "Um operador logico nao pode ser usado com o tipo " ^ (nome_tipo t) in
                   failwith (msg_erro_pos (snd op) msg))
        in
        let op = fst op in
        let tinf = (match (classifica op) with
                      Aritimetico -> verifica_aritimetico ()
                    | Relacional -> verifica_relacional ()
                    | Logico -> verifica_logico ()) in
        (T.ExpOp ((op, tinf), (esq, tesq), (dir, tdir)), tinf)
    | ExpFun (nome, args) ->
        (* Verifica os tipos dos parametros. *)
        let rec verifica_parametros args ps fs =
            match (args, ps, fs) with
              (a::args), (p::ps), (f::fs) ->
                  let mesmo_tipo (posicao a)
                                 "O parametro eh do tipo %s mas deveria ser do tipo %s"
                                 p
                                 f
                  in
                  verifica_parametros args ps fs
            | [], [], [] -> ()
            | _ -> failwith (msg_erro nome "Numero incorreto de parametros.")
        in
        let id = fst nome in
        try
            begin
                let open Amb in
                match (Amb.busca amb id) with
                  (* Verifica se o nome está associado a uma função. *)
                  Amb.EntFun {tipo_fn; formais} ->
                    (* Infere o tipo de cada um dos argumentos. *)
                    let argst = List.map (infere_exp amb) args
                    (* Obtem o tipo de cada parâmetro formal *)
                    and tipos_formais = List.map snd formais in
                    (* Verifica se o tipo de cada argumento confere com o tipo declarado *)
                    (* do parâmetro formal correspondente.                               *)
                    let _ = verifica_parametros args (List.map snd argst) tipos_formais
                    in (T.ExpChamada (id, (List.map fst argst), tipo_fn), tipo_fn)
                | Amb.EntVar -> 
                    (* Se estiver associada a uma variável, falhe *)
                    let msg = id ^ " eh uma variavel e nao uma funcao" in
                    failwith (msg_erro nome msg)
            end
        with Not_found ->
            let msg = id ^ " eh uma variavel e nao uma funcao!" in
            failwith (msg_erro nome msg)


(* Verifica as expressoes da linguagem. *)
let rec verifica_cmd amb tiporet cmd =
    let open A in
    match cmd with
      CmdReturn exp ->
       (match exp with
          (* Se a função não retornar nada, verifica se ela foi declarada como void *)
          None -> 
            let _ = mesmo_tipo (Lexing.dummy_pos)
                               "O tipo retornado eh %s mas foi declarado como %s"
                               Void
                               tiporet
            in
            CmdReturn None
        | Some e ->
            (* Verifica se o tipo inferido para a expressão de retorno confere com o *)
            (* tipo declarado para a função.                                         *)
            let (e1,tinf) = infere_exp amb e in
            let _ = mesmo_tipo (posicao e)
                               "O tipo retornado eh %s mas foi declarado como %s"
                               tinf
                               tiporet
            in
            CmdReturn (Some e1))
    | CmdSe (teste, entao, senao) ->
        let (teste1,tinf) = infere_exp amb teste in
        (* O tipo inferido para a expressão 'teste' do condicional deve ser booleano *)
        let _ = mesmo_tipo (posicao teste)
                           "O teste do if deveria ser do tipo %s e nao %s"
                           Bool
                           tinf
        in
        (* Verifica a validade de cada comando do bloco 'então' *)
        let entao1 = List.map (verifica_cmd amb tiporet) entao in
        (* Verifica a validade de cada comando do bloco 'senão', se houver *)
        let senao1 =
            match senao with
              None -> None
            | Some bloco -> Some (List.map (verifica_cmd amb tiporet) bloco)
        in
        CmdSe (teste1, entao1, senao1)
    | CmdAtrib (elem, exp) ->
        (* Infere o tipo da expressão no lado direito da atribuição *)
        let (exp,  tdir) = infere_exp amb exp
        (* Faz o mesmo para o lado esquerdo *)
        and (elem1, tesq) = infere_exp amb elem in
        (* Os dois tipos devem ser iguais *)
        let _ = mesmo_tipo (posicao elem)
                           "Atribuicao com tipos diferentes: %s = %s"
                           tesq
                           tdir
        in
        CmdAtrib (elem1, exp)
    | CmdFun exp ->
        let (exp,tinf) = infere_exp amb exp in
        CmdChamada exp
        
        
            































