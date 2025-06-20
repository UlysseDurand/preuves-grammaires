#!/bin/bash
set -e

src="$SRC_DIR"
wd="$DEST_DIR"

cp ${wd}/main.ml ${src}/

cat ${src}/typesetutiles.ml > ${src}/tout.ml
cat ${src}/preuveautomatique.ml >> ${src}/tout.ml
cat ${src}/simplifie.ml >> ${src}/tout.ml
cat ${src}/main.ml >> ${src}/tout.ml
echo '#use "tout.ml";;' > ${src}/tmp.ml

cd ${src} && opam exec -- ocaml < ${src}/tmp.ml > ${wd}/output.txt