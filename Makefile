all: aiger.cma aiger.cmx aiger.cmo aigerImperative.cma aigerImperative.cmx aigerImperative.cmo

aiger.cma:
	ocamlbuild -lib str aiger.cma

aiger.cmx:
	ocamlbuild -lib str aiger.cmx

aiger.cmo:
	ocamlbuild -lib str aiger.cmo


aigerImperative.cma:
	ocamlbuild -lib str aigerImperative.cma

aigerImperative.cmx:
	ocamlbuild -lib str  aigerImperative.cmx

aigerImperative.cmo:
	ocamlbuild -lib str  aigerImperative.cmo

examples: compose.native rename.native verilog.native minispec.native

compose.native: examples/compose.ml
	ocamlbuild -lib str examples/compose.native

rename.native: examples/rename.ml
	ocamlbuild -lib str examples/rename.native

verilog.native: examples/verilog.ml
	ocamlbuild -lib str examples/verilog.native

minispec.native: examples/minispec.ml
	ocamlbuild -lib str examples/minispec.native

install: aiger.cma aiger.cmx aiger.cmo
	ocamlfind install aiger META aiger.mli _build/aiger.cmi _build/aiger.cma _build/aiger.cmx _build/aiger.cmo _build/aiger.o

uninstall:
	ocamlfind remove aiger

clean: 
	ocamlbuild -clean

doc: doc/Aiger.html

doc/Aiger.html: aiger.mli 
	ocamldoc -html aiger.mli -d doc

.phony: install uninstall


