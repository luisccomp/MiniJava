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
    | E
    | Not -> Logico
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
    | S.ExpBool b -> (T.ExpBool (fst b, A.Bool), A.Bool)
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
    | S.ExpFun (nome, args) ->
        (* Verifica os tipos dos parametros. *)
        let rec verifica_parametros args ps fs =
            match (args, ps, fs) with
              (a::args), (p::ps), (f::fs) ->
                  let _ = mesmo_tipo (posicao a)
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
                    in (T.ExpFun (id, (List.map fst argst), tipo_fn), tipo_fn)
                | Amb.EntVar _ -> 
                    (* Se estiver associada a uma variável, falhe *)
                    let msg = id ^ " eh uma variavel e nao uma funcao" in
                    failwith (msg_erro nome msg)
            end
        with Not_found ->
            let msg = id ^ " eh uma variavel e nao uma funcao!" in
            failwith (msg_erro nome msg)
    | S.ExpUn (op, dir) ->
        let (dir, tdir) = infere_exp amb dir in (* Inferindo o tipo da expressão a direita do operador *)
       (match (fst op) with
          A.Sub ->
           (match tdir with
              A.Int
            | A.Float ->
                (* O tipo da expressão como um todo é o da expressão da direita *)
                let op = fst op in
                (T.ExpUn ((op, tdir), (dir, tdir)), tdir)
            | t ->
                let msg = "Um operador menos unario nao pode ser usado com o tipo " ^ (nome_tipo t) in
                failwith (msg_erro_pos (snd pos) msg))
        | A.Not ->
           (match tdir with
              A.Bool ->
                let op = fst op in
                (* O operador not so possui uma alternativa de tipo *)
                (T.ExpUn ((op, tdir), (dir, tdir)), tdir)
            | t ->
                let msg = "Um operador not nao pode ser usado com o tipo " ^ (nome_tipo t) in
                failwith (msg_erro_pos (snd pos) msg))
        | _ -> failwith "Operador invalido.")


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
    | CmdIf (teste, entao, senao) ->
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
        CmdIf (teste1, entao1, senao1)
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
        let (exp, tinf) = infere_exp amb exp in
        CmdFun exp
    | CmdReadInt exp ->
        (* O comando de leitura so pode ser usado com inteiros. *)
        let (exp1, tinf) = infere_exp amb exp in
        let _ = mesmo_tipo (posicao exp)
                           "O comando de leitura esperava tipo %s mas foi usado tipo %s"
                           A.Int
                           tinf
        in
        CmdReadInt exp1
    | CmdReadFloat exp ->
        (* O comando de leitura de float so pode ser usado com floats. *)
        let (exp1, tinf) = infere_exp amb exp in
        let _ = mesmo_tipo (posicao exp)
                           "O comando de leitura esperava tipo %s mas foi usado tipo %s"
                           A.Float
                           tinf
        in
        CmdReadFloat exp1
    | CmdReadString exp ->
        (* O comando de leitura de string so aceita string. *)
        let (exp1, tinf) = infere_exp amb exp in
        let _ = mesmo_tipo (posicao exp)
                           "O comando de leitura esperava tipo %s mas foi usado tipo %s"
                           A.String
                           tinf
        in
        CmdReadString exp1
    | CmdReadChar exp ->
        (* O comando de leitura de char so aceita char. *)
        let (exp1, tinf) = infere_exp amb exp in
        let _ = mesmo_tipo (posicao exp)
                           "O comando de leitura esperava tipo %s mas foi usado tipo %s"
                           A.Char
                           tinf
        in
        CmdReadChar exp1
    | CmdFor (init, teste, fim, comandos) -> 
        let init1 = verifica_cmd amb tiporet init in
        let (teste1, tinf) = infere_exp amb teste in
        (* O teste do for deve ser do tipo booleano. *)
        let _ = mesmo_tipo (posicao teste)
                           "O teste do for deveria ser do tipo %s e nao %s"
                           A.Bool
                           tinf
        in
        let fim1 = verifica_cmd amb tiporet fim in
        let comandos1 = List.map (verifica_cmd amb tiporet) comandos in
        CmdFor (init1, teste1, fim1, comandos1)
    | CmdWhile (teste, comandos) ->
        let (teste1, tinf) = infere_exp amb teste in
        (* O teste do while deve ser booleano. *)
        let _ = mesmo_tipo (posicao teste)
                           "O teste do while deveria ser do tipo %s e nao %s"
                           A.Bool
                           tinf
        in
        let comandos1 = List.map (verifica_cmd amb tiporet) comandos in
        CmdWhile (teste1, comandos1)
    | CmdSwitch (teste, cases, default) ->
        (* Verifica os comandos "case" dentro do switch. *)
        let rec verifica_case amb tiporet tipovar cmd =
            match cmd with
              Case (exp, comandos) ->
                let exp1, tinf = infere_exp amb exp in
                (* O tipo usado na inicialização do switch e o tipo usado para testes no case devem ser iguais *)
                let _ = mesmo_tipo (posicao exp)
                                   "Foi usado uma expressao do tipo %s no case mas esperava %s"
                                   tipovar
                                   tinf
                in
                let comandos1 = List.map (verifica_cmd amb tiporet) comandos in
                Case (exp1, comandos1)
            | _ -> failwith "Comando desconhecido."
        in
        let teste1, tinf = infere_exp amb teste in
        let cases1 = List.map (verifica_case amb tiporet tinf) cases in
        let default1 = match default with
                         None -> None
                       | Some (Default bloco) -> Some (Default (List.map (verifica_cmd amb tiporet) bloco))
        in
        CmdSwitch (teste1, cases1, default1)
    | CmdPrint expressoes ->
        (* Verifica o tipo de cada argumento da função de saída. *)
        let exps = List.map (infere_exp amb) expressoes in
        CmdPrint (List.map fst exps)
    | _ -> failwith "Semantico: comando desconhecido."


and verifica_fun amb ast =
    let open A in
    match ast with
      A.DecFun {fn_nome; fn_tiporet; fn_formais; fn_locais; fn_corpo} ->
        (* Estende o ambiente global, adicionando um ambiente local *)
        let ambfn = Amb.novo_escopo amb in
        (* Insere os parâmetros no novo ambiente *)
        let insere_parametro (v, t) = Amb.insere_param ambfn (fst v) t in
        let _ = List.iter insere_parametro fn_formais in
        (* Insere as variaveis locais no novo ambiente *)
        let insere_local = function
            DecVar (v, t) -> Amb.insere_local ambfn (fst v) t in
        let _ = List.iter insere_local fn_locais in
        (* Verifica cada comando presente no corpo da função usando o novo ambiente *)
        let corpo_tipado = List.map (verifica_cmd ambfn fn_tiporet) fn_corpo in
        A.DecFun {fn_nome; fn_tiporet; fn_formais; fn_locais; fn_corpo=corpo_tipado}


let rec verifica_dup xs =
    match xs with
      [] -> []
    | (nome, t)::xs ->
        let id = fst nome in
        if (List.for_all (fun (n, t) -> (fst n) <> id) xs) then
            (id, t) :: verifica_dup xs
        else
            let msg = "Parametro duplicado " ^ id in
            failwith (msg_erro nome msg)


(* Insere declaração de variável no ambiente. *)
let insere_declaracao_var amb dec =
    let open A in
    match dec with
      DecVar (nome, tipo) -> Amb.insere_local amb (fst nome) tipo


let insere_declaracao_fun amb dec =
    let open A in
    match dec with
      DecFun {fn_nome; fn_tiporet; fn_formais; fn_corpo} ->
        (* Verifica se não há parâmetros duplicados. *)
        let formais = verifica_dup fn_formais in
        let nome = fst fn_nome in
        Amb.insere_fun amb nome formais fn_tiporet


(* Lista de cabeçalho das funções pré definidas. *)
let fn_predefs = let open A in []

(* Insere as funções pré definidas no ambiente global. *)
let declara_predefinidas amb =
    List.iter (fun (n, ps, tr) -> Amb.insere_fun amb n ps tr) fn_predefs
        
        
let semantico ast =
    (* Cria o ambiente global inicialmente vazio. *)
    let amb_global = Amb.novo_amb [] in
    let _ = declara_predefinidas amb_global in
    let (A.Programa decs_funs) = ast in
    let _ = List.iter (insere_declaracao_fun amb_global) decs_funs in
    (* Verificação de tipo nas funções. *)
    let decs_funs = List.map (verifica_fun amb_global) decs_funs in
    (A.Programa decs_funs, amb_global)
