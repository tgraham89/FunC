
scanner: scanner.mll
	ocamlbuild scanner.native

parser: parser.mly
	ocamlyacc -v parser.mly

.PHONY: clean
clean:
	rm parser.output