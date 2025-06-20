FROM ocaml/opam:debian

USER root

ENV SRC_DIR=/app-src \
    DEST_DIR=/app

RUN mkdir -p $SRC_DIR $DEST_DIR

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh
COPY typesetutiles.ml preuveautomatique.ml simplifie.ml $SRC_DIR/
COPY Makefile $SRC_DIR/

WORKDIR $DEST_DIR

CMD [ "/entrypoint.sh" ]