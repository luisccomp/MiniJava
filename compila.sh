
#menhir -v --list-errors sintatico.mly > sintatico.msg
#menhir -v sintatico.mly --compile-errors sintatico.msg > erroSint.ml
#ocamlbuild -use-ocamlfind -use-menhir -menhir "menhir --table" -package menhirLib sintaticoTest.byte

menhir -v --list-errors sintatico.mly > sintatico.messages
menhir -v --list-errors sintatico.mly --compile-errors sintatico.messages > fnmes.ml

ocamlbuild -use-ocamlfind -use-menhir -menhir "menhir --table" -package menhirLib semanticoTest.byte

