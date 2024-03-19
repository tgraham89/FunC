build: scanner parser ast

scanner: scanner.mll
	ocamlbuild scanner.native

parser: parser.mly
	ocamlyacc -v parser.mly

ast: ast.ml
	ocamlopt -o a.out ast.ml

.PHONY: clean

clean:
	rm -f parser.ml
	rm -f parser.mli
	rm -f parser.output
	rm -f scanner.native
	rm -f *.out
	rm -f *.o
	rm -f *.cmi
	rm -f *.cmx
	rm -rf _build
