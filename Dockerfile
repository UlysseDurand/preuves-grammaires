FROM ocaml/opam:latest

USER root

ENV SRC_DIR=/app-src \
    WORK_DIR=/app

RUN mkdir -p $SRC_DIR $DEST_DIR

COPY deployment/str2str/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh
COPY src/* $SRC_DIR/

WORKDIR $WORK_DIR

CMD [ "/entrypoint.sh" ]