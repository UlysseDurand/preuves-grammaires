toplevel:
	@make -s tout
	@echo "#use \"tout.ml\";;" > tmp.ml
	@ocaml < tmp.ml
	@rm tmp.ml

tout:
	@cat typesetutiles.ml > tout.ml
	@cat preuveautomatique.ml >> tout.ml
	@cat simplifie.ml >> tout.ml
	@cat main.ml >> tout.ml

all:
	@make -s tout
	@make -s compile
	@make -s execute

compile:
	@ocamlopt tout.ml -o main
	@make -s clean

execute:
	@./main

clean:
	@rm -f *.cmi
	@rm -f *.cmx
	@rm -f *.cmo
	@rm -f *.o
