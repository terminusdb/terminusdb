FROM terminusdb/terminus_store_prolog:v0.14.3
WORKDIR /usr/share/swi-prolog/pack/jwt_io
RUN export BUILD_DEPS="git build-essential make libjwt-dev libssl-dev pkg-config" \
        && apt-get update && apt-get install $BUILD_DEPS -y --no-install-recommends \
        && git clone https://github.com/terminusdb-labs/jwt_io.git ./ && make

FROM terminusdb/terminus_store_prolog:v0.14.3
WORKDIR /app/terminusdb
COPY ./ /app/terminusdb
COPY --from=0 /usr/share/swi-prolog/pack/jwt_io /usr/share/swi-prolog/pack/jwt_io
ENV TERMINUSDB_JWT_ENABLED=true
RUN cp /usr/share/swi-prolog/pack/jwt_io/jwt_io.so src/ \
        && apt-get update && apt-get install -y --no-install-recommends libjwt0 make \
        && rm -rf /var/cache/apt/* && rm -rf /var/lib/apt/lists/* && make
CMD ["/app/terminusdb/distribution/init_docker.sh"]
