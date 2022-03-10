toplevel:
	@echo "#use \"main.ml\";;" > tmp.ml
	@ocaml < tmp.ml
	@rm tmp.ml

all:
	@make -s compile
	@make -s execute

compile:
	@ocamlopt typesetutiles.ml verifalge.ml preuveautomatique.ml main.ml -o main
	@make -s clean

execute:
	@./main

clean:
	@rm -f *.cmi
	@rm -f *.cmx
	@rm -f *.cmo
	@rm -f *.o