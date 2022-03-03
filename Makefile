toplevel:
	echo "#use \"main2.ml\";;" > tmp.ml
	ocaml < tmp.ml
	rm tmp.ml
