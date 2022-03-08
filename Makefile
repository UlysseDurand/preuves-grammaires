toplevel:
	echo "#use \"main.ml\";;" > tmp.ml
	ocaml < tmp.ml
	rm tmp.ml
