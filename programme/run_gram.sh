#!/bin/bash

TMP_SCRIPT=/tmp/ocaml_run_with_types_XXXX.ml

FILES=("typesetutiles.ml" "preuveautomatique.ml" "simplifie.ml" "$1")

for f in "${FILES[@]}"; do
  echo "(* >>> Loading $f *)" >> "$TMP_SCRIPT"
  cat "$f" >> "$TMP_SCRIPT"
  echo "" >> "$TMP_SCRIPT"
done

echo "🔍 Running OCaml with types:"
echo
ocaml "$TMP_SCRIPT"
