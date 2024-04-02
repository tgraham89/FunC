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

ast_test:
	ocamlbuild -I src test/ast_test.native

sast_test:
	ocamlbuild -I src test/sast_test.native

tests: sast_test ast_test

hello_world:
	ocamlbuild -I src test/sast_test.native
	./sast_test.native < test/hello_world.tb

for_loop:
	ocamlbuild -I src test/sast_test.native
	./sast_test.native < test/for_loop.tb

.PHONY: clean
clean:
	rm -f src/parser.ml
	rm -f src/parser.mli
	rm -f src/parser.output
	rm -f src/*.native
	rm -f *.native
	rm -rf _build
	rm -f src/ast.cmi
	rm -f src/ast.cmo
	rm -f test/ast_test.cmi
	rm -f test/ast_test.cmo
	rm -f ./test/*.cmi
	rm -f ./test/*.cmo
	rm -f ./test/*.native
	rm -f ./test/*.out
	rm -f hello_world.output

.PHONY: test test_ast test_scanner
test: test_ast test_scanner
	
test_ast:
	ocamlc -o ./test/test_ast.native src/ast.ml ./test/test_ast.ml
	rm -f ./test/test_ast.cmi
	rm -f ./test/test_ast.cmo
	./test/test_ast.native > ./test/test_ast.out

test_scanner:
	rm -f ast.cmi
	rm -f ast.cmo
	ocamlbuild ./test/test_scanner.native
	mv test_scanner.native ./test/
	./test/test_scanner.native > ./test/test_scanner.out
