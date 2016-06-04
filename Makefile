all: aiger.cma aiger.cmx aiger.cmo 

aiger.cma:
	ocamlbuild -lib str aiger.cma

aiger.cmx:
	ocamlbuild -lib str aiger.cmx

aiger.cmo:
	ocamlbuild -lib str aiger.cmo


aigerImperative.cma:
	ocamlbuild -lib str -package batteries aigerImperative.cma

aigerImperative.cmx:
	ocamlbuild -lib str -package batteries  aigerImperative.cmx

aigerImperative.cmo:
	ocamlbuild -lib str -package batteries  aigerImperative.cmo

compose.native:
	ocamlbuild -lib str compose.native

rename.native:
	ocamlbuild -lib str -package batteries rename.native

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


