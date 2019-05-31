programa: PROGRAMA ID DPTOS ARRANJO ACOL INT PPTO INT FCOL DE VIRG 
##
## Ends in an error in state: 20.
##
## tipo_arranjo -> ARRANJO ACOL limites FCOL DE . tipo [ PTV ]
##
## The known suffix of the stack is as follows:
## ARRANJO ACOL limites FCOL DE 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS ARRANJO ACOL INT PPTO INT FCOL VIRG 
##
## Ends in an error in state: 19.
##
## tipo_arranjo -> ARRANJO ACOL limites FCOL . DE tipo [ PTV ]
##
## The known suffix of the stack is as follows:
## ARRANJO ACOL limites FCOL 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS ARRANJO ACOL INT PPTO INT VIRG 
##
## Ends in an error in state: 18.
##
## tipo_arranjo -> ARRANJO ACOL limites . FCOL DE tipo [ PTV ]
##
## The known suffix of the stack is as follows:
## ARRANJO ACOL limites 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS ARRANJO ACOL INT PPTO VIRG 
##
## Ends in an error in state: 16.
##
## limites -> INT PPTO . INT [ FCOL ]
##
## The known suffix of the stack is as follows:
## INT PPTO 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS ARRANJO ACOL INT VIRG 
##
## Ends in an error in state: 15.
##
## limites -> INT . PPTO INT [ FCOL ]
##
## The known suffix of the stack is as follows:
## INT 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS ARRANJO ACOL VIRG 
##
## Ends in an error in state: 14.
##
## tipo_arranjo -> ARRANJO ACOL . limites FCOL DE tipo [ PTV ]
##
## The known suffix of the stack is as follows:
## ARRANJO ACOL 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS ARRANJO VIRG 
##
## Ends in an error in state: 13.
##
## tipo_arranjo -> ARRANJO . ACOL limites FCOL DE tipo [ PTV ]
##
## The known suffix of the stack is as follows:
## ARRANJO 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS BOOLEANO PTV VIRG 
##
## Ends in an error in state: 107.
##
## list(declaracao) -> declaracao . list(declaracao) [ INICIO ]
##
## The known suffix of the stack is as follows:
## declaracao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS INTEIRO VIRG 
##
## Ends in an error in state: 31.
##
## declaracao -> separated_nonempty_list(VIRG,ID) DPTOS tipo . PTV [ INICIO ID ]
##
## The known suffix of the stack is as follows:
## separated_nonempty_list(VIRG,ID) DPTOS tipo 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS REGISTRO ID DPTOS BOOLEANO PTV FIM VIRG 
##
## Ends in an error in state: 29.
##
## tipo_registro -> REGISTRO nonempty_list(__anonymous_0) FIM . REGISTRO [ PTV ]
##
## The known suffix of the stack is as follows:
## REGISTRO nonempty_list(__anonymous_0) FIM 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS REGISTRO ID DPTOS INTEIRO PTV VIRG 
##
## Ends in an error in state: 26.
##
## nonempty_list(__anonymous_0) -> ID DPTOS tipo PTV . [ FIM ]
## nonempty_list(__anonymous_0) -> ID DPTOS tipo PTV . nonempty_list(__anonymous_0) [ FIM ]
##
## The known suffix of the stack is as follows:
## ID DPTOS tipo PTV 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS REGISTRO ID DPTOS INTEIRO VIRG 
##
## Ends in an error in state: 25.
##
## nonempty_list(__anonymous_0) -> ID DPTOS tipo . PTV [ FIM ]
## nonempty_list(__anonymous_0) -> ID DPTOS tipo . PTV nonempty_list(__anonymous_0) [ FIM ]
##
## The known suffix of the stack is as follows:
## ID DPTOS tipo 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS REGISTRO ID DPTOS VIRG 
##
## Ends in an error in state: 9.
##
## nonempty_list(__anonymous_0) -> ID DPTOS . tipo PTV [ FIM ]
## nonempty_list(__anonymous_0) -> ID DPTOS . tipo PTV nonempty_list(__anonymous_0) [ FIM ]
##
## The known suffix of the stack is as follows:
## ID DPTOS 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS REGISTRO ID VIRG 
##
## Ends in an error in state: 8.
##
## nonempty_list(__anonymous_0) -> ID . DPTOS tipo PTV [ FIM ]
## nonempty_list(__anonymous_0) -> ID . DPTOS tipo PTV nonempty_list(__anonymous_0) [ FIM ]
##
## The known suffix of the stack is as follows:
## ID 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS REGISTRO VIRG 
##
## Ends in an error in state: 7.
##
## tipo_registro -> REGISTRO . nonempty_list(__anonymous_0) FIM REGISTRO [ PTV ]
##
## The known suffix of the stack is as follows:
## REGISTRO 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID DPTOS VIRG 
##
## Ends in an error in state: 6.
##
## declaracao -> separated_nonempty_list(VIRG,ID) DPTOS . tipo PTV [ INICIO ID ]
##
## The known suffix of the stack is as follows:
## separated_nonempty_list(VIRG,ID) DPTOS 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID STRING 
##
## Ends in an error in state: 2.
##
## separated_nonempty_list(VIRG,ID) -> ID . [ DPTOS ]
## separated_nonempty_list(VIRG,ID) -> ID . VIRG separated_nonempty_list(VIRG,ID) [ DPTOS ]
##
## The known suffix of the stack is as follows:
## ID 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA ID VIRG VIRG 
##
## Ends in an error in state: 3.
##
## separated_nonempty_list(VIRG,ID) -> ID VIRG . separated_nonempty_list(VIRG,ID) [ DPTOS ]
##
## The known suffix of the stack is as follows:
## ID VIRG 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO ENTRADA VIRG 
##
## Ends in an error in state: 81.
##
## comando_entrada -> ENTRADA . separated_nonempty_list(VIRG,variavel) PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
##
## The known suffix of the stack is as follows:
## ENTRADA 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO FIM PTV VIRG 
##
## Ends in an error in state: 103.
##
## programa -> PROGRAMA list(declaracao) INICIO list(comando) FIM PTV . EOF [ # ]
##
## The known suffix of the stack is as follows:
## PROGRAMA list(declaracao) INICIO list(comando) FIM PTV 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO FIM VIRG 
##
## Ends in an error in state: 102.
##
## programa -> PROGRAMA list(declaracao) INICIO list(comando) FIM . PTV EOF [ # ]
##
## The known suffix of the stack is as follows:
## PROGRAMA list(declaracao) INICIO list(comando) FIM 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO ID ACOL STRING VIRG 
##
## Ends in an error in state: 46.
##
## expressao -> expressao . MAIS expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FCOL ELOG DIV DIFER CONCAT ]
## variavel -> variavel ACOL expressao . FCOL [ VIRG PTV PTO OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ATRIB ACOL ]
##
## The known suffix of the stack is as follows:
## variavel ACOL expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO ID ACOL VIRG 
##
## Ends in an error in state: 45.
##
## variavel -> variavel ACOL . expressao FCOL [ VIRG PTV PTO OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ATRIB ACOL ]
##
## The known suffix of the stack is as follows:
## variavel ACOL 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO ID ATRIB STRING VIRG 
##
## Ends in an error in state: 86.
##
## comando_atribuicao -> variavel ATRIB expressao . PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
## expressao -> expressao . MAIS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## variavel ATRIB expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO ID ATRIB VIRG 
##
## Ends in an error in state: 85.
##
## comando_atribuicao -> variavel ATRIB . expressao PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
##
## The known suffix of the stack is as follows:
## variavel ATRIB 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO ID PTO VIRG 
##
## Ends in an error in state: 43.
##
## variavel -> variavel PTO . ID [ VIRG PTV PTO OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ATRIB ACOL ]
##
## The known suffix of the stack is as follows:
## variavel PTO 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO ID VIRG 
##
## Ends in an error in state: 84.
##
## comando_atribuicao -> variavel . ATRIB expressao PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
## variavel -> variavel . PTO ID [ PTO ATRIB ACOL ]
## variavel -> variavel . ACOL expressao FCOL [ PTO ATRIB ACOL ]
##
## The known suffix of the stack is as follows:
## variavel 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SAIDA ID PTV VIRG 
##
## Ends in an error in state: 105.
##
## list(comando) -> comando . list(comando) [ FIM ]
##
## The known suffix of the stack is as follows:
## comando 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SAIDA ID STRING 
##
## Ends in an error in state: 76.
##
## separated_nonempty_list(VIRG,variavel) -> variavel . [ PTV ]
## separated_nonempty_list(VIRG,variavel) -> variavel . VIRG separated_nonempty_list(VIRG,variavel) [ PTV ]
## variavel -> variavel . PTO ID [ VIRG PTV PTO ACOL ]
## variavel -> variavel . ACOL expressao FCOL [ VIRG PTV PTO ACOL ]
##
## The known suffix of the stack is as follows:
## variavel 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SAIDA ID VIRG VIRG 
##
## Ends in an error in state: 77.
##
## separated_nonempty_list(VIRG,variavel) -> variavel VIRG . separated_nonempty_list(VIRG,variavel) [ PTV ]
##
## The known suffix of the stack is as follows:
## variavel VIRG 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SAIDA VIRG 
##
## Ends in an error in state: 75.
##
## comando_saida -> SAIDA . separated_nonempty_list(VIRG,variavel) PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
##
## The known suffix of the stack is as follows:
## SAIDA 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR APAR STRING VIRG 
##
## Ends in an error in state: 70.
##
## expressao -> expressao . MAIS expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> APAR expressao . FPAR [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## APAR expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR APAR VIRG 
##
## Ends in an error in state: 41.
##
## expressao -> APAR . expressao FPAR [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## APAR 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR ID VIRG 
##
## Ends in an error in state: 42.
##
## expressao -> variavel . [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## variavel -> variavel . PTO ID [ PTV PTO OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ACOL ]
## variavel -> variavel . ACOL expressao FCOL [ PTV PTO OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ACOL ]
##
## The known suffix of the stack is as follows:
## variavel 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING CONCAT STRING VIRG 
##
## Ends in an error in state: 60.
##
## expressao -> expressao . MAIS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao CONCAT expressao . [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao CONCAT expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING CONCAT VIRG 
##
## Ends in an error in state: 59.
##
## expressao -> expressao CONCAT . expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao CONCAT 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING DIFER STRING VIRG 
##
## Ends in an error in state: 68.
##
## expressao -> expressao . MAIS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao DIFER expressao . [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao DIFER expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING DIFER VIRG 
##
## Ends in an error in state: 67.
##
## expressao -> expressao DIFER . expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao DIFER 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING DIV VIRG 
##
## Ends in an error in state: 53.
##
## expressao -> expressao DIV . expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao DIV 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING ELOG STRING VIRG 
##
## Ends in an error in state: 66.
##
## expressao -> expressao . MAIS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao ELOG expressao . [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao ELOG expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING ELOG VIRG 
##
## Ends in an error in state: 65.
##
## expressao -> expressao ELOG . expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao ELOG 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING FPAR ENTAO SAIDA ID PTV FIM SE VIRG 
##
## Ends in an error in state: 99.
##
## comando_se -> SE APAR expressao FPAR ENTAO nonempty_list(comando) option(__anonymous_1) FIM SE . PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
##
## The known suffix of the stack is as follows:
## SE APAR expressao FPAR ENTAO nonempty_list(comando) option(__anonymous_1) FIM SE 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING FPAR ENTAO SAIDA ID PTV FIM VIRG 
##
## Ends in an error in state: 98.
##
## comando_se -> SE APAR expressao FPAR ENTAO nonempty_list(comando) option(__anonymous_1) FIM . SE PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
##
## The known suffix of the stack is as follows:
## SE APAR expressao FPAR ENTAO nonempty_list(comando) option(__anonymous_1) FIM 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING FPAR ENTAO SAIDA ID PTV SENAO SAIDA ID PTV SENAO 
##
## Ends in an error in state: 97.
##
## comando_se -> SE APAR expressao FPAR ENTAO nonempty_list(comando) option(__anonymous_1) . FIM SE PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
##
## The known suffix of the stack is as follows:
## SE APAR expressao FPAR ENTAO nonempty_list(comando) option(__anonymous_1) 
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 95, spurious reduction of production nonempty_list(comando) -> comando 
## In state 90, spurious reduction of production option(__anonymous_1) -> SENAO nonempty_list(comando) 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING FPAR ENTAO SAIDA ID PTV SENAO VIRG 
##
## Ends in an error in state: 89.
##
## option(__anonymous_1) -> SENAO . nonempty_list(comando) [ FIM ]
##
## The known suffix of the stack is as follows:
## SENAO 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING FPAR ENTAO SAIDA ID PTV VIRG 
##
## Ends in an error in state: 95.
##
## nonempty_list(comando) -> comando . [ SENAO FIM ]
## nonempty_list(comando) -> comando . nonempty_list(comando) [ SENAO FIM ]
##
## The known suffix of the stack is as follows:
## comando 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING FPAR ENTAO VIRG 
##
## Ends in an error in state: 74.
##
## comando_se -> SE APAR expressao FPAR ENTAO . nonempty_list(comando) option(__anonymous_1) FIM SE PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
##
## The known suffix of the stack is as follows:
## SE APAR expressao FPAR ENTAO 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING FPAR VIRG 
##
## Ends in an error in state: 73.
##
## comando_se -> SE APAR expressao FPAR . ENTAO nonempty_list(comando) option(__anonymous_1) FIM SE PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
##
## The known suffix of the stack is as follows:
## SE APAR expressao FPAR 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING IGUAL STRING VIRG 
##
## Ends in an error in state: 64.
##
## expressao -> expressao . MAIS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao IGUAL expressao . [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao IGUAL expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING IGUAL VIRG 
##
## Ends in an error in state: 63.
##
## expressao -> expressao IGUAL . expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao IGUAL 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING MAIOR STRING VIRG 
##
## Ends in an error in state: 62.
##
## expressao -> expressao . MAIS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao MAIOR expressao . [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao MAIOR expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING MAIOR VIRG 
##
## Ends in an error in state: 61.
##
## expressao -> expressao MAIOR . expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao MAIOR 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING MAIS STRING VIRG 
##
## Ends in an error in state: 58.
##
## expressao -> expressao . MAIS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao MAIS expressao . [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao MAIS expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING MAIS VIRG 
##
## Ends in an error in state: 57.
##
## expressao -> expressao MAIS . expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao MAIS 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING MENOR STRING VIRG 
##
## Ends in an error in state: 56.
##
## expressao -> expressao . MAIS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao MENOR expressao . [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao MENOR expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING MENOR VIRG 
##
## Ends in an error in state: 55.
##
## expressao -> expressao MENOR . expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao MENOR 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING MENOS STRING VIRG 
##
## Ends in an error in state: 52.
##
## expressao -> expressao . MAIS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao MENOS expressao . [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao MENOS expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING MENOS VIRG 
##
## Ends in an error in state: 51.
##
## expressao -> expressao MENOS . expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao MENOS 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING MULT VIRG 
##
## Ends in an error in state: 49.
##
## expressao -> expressao MULT . expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao MULT 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING OULOG STRING VIRG 
##
## Ends in an error in state: 48.
##
## expressao -> expressao . MAIS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao OULOG expressao . [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao OULOG expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING OULOG VIRG 
##
## Ends in an error in state: 47.
##
## expressao -> expressao OULOG . expressao [ PTV OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR FCOL ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## expressao OULOG 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR STRING VIRG 
##
## Ends in an error in state: 72.
##
## comando_se -> SE APAR expressao . FPAR ENTAO nonempty_list(comando) option(__anonymous_1) FIM SE PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
## expressao -> expressao . MAIS expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOS expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MULT expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIV expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MENOR expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . IGUAL expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . DIFER expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . MAIOR expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . ELOG expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . OULOG expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
## expressao -> expressao . CONCAT expressao [ OULOG MULT MENOS MENOR MAIS MAIOR IGUAL FPAR ELOG DIV DIFER CONCAT ]
##
## The known suffix of the stack is as follows:
## SE APAR expressao 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE APAR VIRG 
##
## Ends in an error in state: 36.
##
## comando_se -> SE APAR . expressao FPAR ENTAO nonempty_list(comando) option(__anonymous_1) FIM SE PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
##
## The known suffix of the stack is as follows:
## SE APAR 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO SE VIRG 
##
## Ends in an error in state: 35.
##
## comando_se -> SE . APAR expressao FPAR ENTAO nonempty_list(comando) option(__anonymous_1) FIM SE PTV [ SENAO SE SAIDA ID FIM ENTRADA ]
##
## The known suffix of the stack is as follows:
## SE 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA INICIO VIRG 
##
## Ends in an error in state: 34.
##
## programa -> PROGRAMA list(declaracao) INICIO . list(comando) FIM PTV EOF [ # ]
##
## The known suffix of the stack is as follows:
## PROGRAMA list(declaracao) INICIO 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: PROGRAMA VIRG 
##
## Ends in an error in state: 1.
##
## programa -> PROGRAMA . list(declaracao) INICIO list(comando) FIM PTV EOF [ # ]
##
## The known suffix of the stack is as follows:
## PROGRAMA 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

programa: VIRG 
##
## Ends in an error in state: 0.
##
## programa' -> . programa [ # ]
##
## The known suffix of the stack is as follows:
## 
##

<YOUR SYNTAX ERROR MESSAGE HERE>

