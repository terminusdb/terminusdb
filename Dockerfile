FROM terminusdb/terminus_store_prolog:v0.14.3
WORKDIR /app/jwt_io
RUN export BUILD_DEPS="git build-essential make libjwt-dev libssl-dev pkg-config" \
        && apt-get update && apt-get install $BUILD_DEPS -y --no-install-recommends \
        && git clone --single-branch --branch v0.0.3-fix-make https://github.com/terminusdb-labs/jwt_io.git ./ \
        && swipl -g "pack_install('file:///app/jwt_io', [interactive(false)])"

FROM terminusdb/terminus_store_prolog:v0.14.3
WORKDIR /app/terminusdb
COPY ./ /app/terminusdb
COPY --from=0 /usr/share/swi-prolog/pack/jwt_io /usr/share/swi-prolog/pack/jwt_io
ENV TERMINUSDB_JWT_ENABLED=true
RUN apt-get update && apt-get install -y --no-install-recommends libjwt0 make \
        && swipl -g "ignore(pack_install('https://github.com/terminusdb-labs/jwt_io.git', [interactive(false)]))" \
    && rm -rf /var/cache/apt/* && rm -rf /var/lib/apt/lists/* && make
CMD ["/app/terminusdb/distribution/init_docker.sh"]
