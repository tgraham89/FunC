build: scanner parser

setup:
	@if ! opam switch list | grep -q '5.1.1'; then \
        opam switch create 5.1.1 || true; \
    else \
        echo "Switch 5.1.1 already exists."; \
    fi
	eval $$(opam config env)
	eval $$(opam env)
	eval $$(opam env --switch=5.1.1)

scanner: scanner.mll
	eval $$(opam env)
	ocamlbuild scanner.native

parser: parser.mly
	ocamlyacc -v parser.mly

test2:
	ocamlbuild test2.native

.PHONY: clean
clean:
	rm -f parser.ml
	rm -f parser.mli
	rm -f parser.output
	rm -f scanner.native
	rm -rf _build
