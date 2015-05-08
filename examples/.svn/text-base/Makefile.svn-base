
# if aiger have been installed in the system we can use ocamlfind
test.of: test.ml
	ocamlfind ocamlc -o test -linkpkg -package aiger test.ml

test: test.ml
	ocamlc -I ../_build/ ../_build/aiger.cma test.ml -o test

test.opt: test.ml
	ocamlfind ocamlopt -o test.opt -linkpkg -package str,aiger test.ml

