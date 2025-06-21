#!/bin/sh
cp -r $SRC_DIR/* /tmp
cp $WORK_DIR/input.txt /tmp
cp $WORK_DIR/input.txt $WORK_DIR/main.ml

# Contruction of the output.txt file
cd /tmp
cat typesetutiles.ml > tmp.ml
cat preuveautomatique.ml >> tmp.ml
cat simplifie.ml >> tmp.ml
cat main.ml >> tmp.ml
echo '#use "tmp.ml";;' > output.txt

cp output.txt $WORK_DIR/output.txt