FROM ocaml/opam:debian

WORKDIR /app
COPY typesetutiles.ml preuveautomatique.ml simplifie.ml .
COPY Makefile .

ENTRYPOINT ["opam", "exec", "--", "make", "toplevel"]
