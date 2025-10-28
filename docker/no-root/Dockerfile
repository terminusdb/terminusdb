# syntax=docker/dockerfile:1.3

ARG BASE_IMAGE=terminusdb/terminusdb-server:local
FROM ${BASE_IMAGE}
RUN chown -R 1000:1000 /app/terminusdb/storage
RUN groupadd -g 1000 terminusdb
RUN useradd -m -u 1000 -g 1000 terminusdb
USER 1000
