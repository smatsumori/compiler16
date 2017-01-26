YACC=ocamlyacc
LEX =ocamllex
CC= ocamlc
FILE = parser.mly lexer.mll sim.ml absyn.ml

simc: $(FILE)
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc -c absyn.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c emitter.ml
	ocamlc -c sim.ml
	ocamlc -o simc unix.cma absyn.cmo lexer.cmo parser.cmo emitter.cmo sim.cmo

clean:
	rm  -f *.cmi *.cmo parser.ml lexer.ml parser.mli simc

