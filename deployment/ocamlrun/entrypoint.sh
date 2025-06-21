#!/bin/sh
cp -r $SRC_DIR/* /tmp
cp $WORK_DIR/input.txt /tmp
cp /tmp/input.txt /tmp/main.ml

# Contruction of the output.txt file
cd /tmp
cat typesetutiles.ml > tout.ml
cat preuveautomatique.ml >> tout.ml
cat simplifie.ml >> tout.ml
cat main.ml >> tout.ml
echo "#use \"tout.ml\";;" > tmp.ml
opam exec ocaml < tmp.ml > output.txt

cp output.txt $WORK_DIR/output.txt