build: scanner parser

setup:
	@if ! opam switch list | grep -q '5.1.1'; then \
        opam switch create 5.1.1 || true; \
    else \
        echo "Switch 5.1.1 already exists."; \
    fi
	opam install ocaml-lsp-server
	opam install ocamlformat
	eval $$(opam config env)
	eval $$(opam env)
	eval $$(opam env --switch=5.1.1)

semant:
	eval $$(opam env)
	ocamlbuild semant.native

scanner: scanner.mll
	eval $$(opam env)
	ocamlbuild scanner.native

parser: parser.mly
	ocamlyacc -v parser.mly

test2:
	ocamlbuild test2.native

test1:
	ocamlbuild test1.native

tests: test1 test2

.PHONY: clean
clean:
	rm -f parser.ml
	rm -f parser.mli
	rm -f parser.output
	rm -f *.native
	rm -rf _build
	rm -rf 
