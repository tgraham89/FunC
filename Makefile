build: clean scanner parser semant

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
	rm -f parser.ml
	rm -f parser.mli

test2:
	ocamlbuild test2.native

test1:
	ocamlbuild test1.native

tests: test1 test2

hello_world:
	ocamlbuild test1.native
	./test1.native < hello_world.tb > hello_world.output


.PHONY: clean
clean:
	rm -f parser.ml
	rm -f parser.mli
	rm -f parser.output
	rm -f *.native
	rm -rf _build
	rm -f ast.cmi
	rm -f ast.cmo
	rm -f ast_test.cmi
	rm -f ast_test.cmo
	rm -f ./test/*.cmi
	rm -f ./test/*.cmo
	rm -f ./test/*.native
	rm -f ./test/*.out
	rm -f hello_world.output

.PHONY: test
test:
	ocamlc -o ./test/test_ast.native ast.ml ./test/test_ast.ml
	./test/test_ast.native > ./test/test_ast.out
