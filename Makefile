build: scanner parser

scanner: scanner.mll
	ocamlbuild scanner.native

parser: parser.mly
	ocamlyacc -v parser.mly

.PHONY: clean

clean:
	rm -f parser.ml
	rm -f parser.mli
	rm -f parser.output
	rm -f scanner.native
	rm -rf _build
