FROM terminusdb/swipl:v8.4.2
ENV TUS_VERSION v0.0.9
WORKDIR /app/pack
RUN export BUILD_DEPS="git build-essential make libjwt-dev libssl-dev pkg-config" \
        && apt-get update && apt-get install $BUILD_DEPS -y --no-install-recommends \
        && git clone --single-branch --branch v0.0.5 https://github.com/terminusdb-labs/jwt_io.git jwt_io \
        && git clone --single-branch --branch $TUS_VERSION https://github.com/terminusdb/tus.git tus \
        && swipl -g "pack_install('file:///app/pack/jwt_io', [interactive(false)])" \
        && swipl -g "pack_install('file:///app/pack/tus', [interactive(false)])"

FROM terminusdb/swipl:v8.4.2
WORKDIR /app/rust
COPY ./src/rust /app/rust
RUN BUILD_DEPS="git build-essential curl clang" && apt-get update \
	&& apt-get install -y --no-install-recommends $BUILD_DEPS \
        ca-certificates
RUN curl https://sh.rustup.rs -sSf | bash -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN cargo build --release

FROM terminusdb/swipl:v8.4.2
WORKDIR /app/terminusdb
COPY ./ /app/terminusdb
COPY --from=0 /root/.local/share/swi-prolog/pack/ /usr/share/swi-prolog/pack
COPY --from=1 /app/rust/target/release/libterminusdb_dylib.so /app/terminusdb/src/rust/librust.so
ARG MAKE_ARGS=""
ARG TERMINUSDB_GIT_HASH=null
ENV TERMINUSDB_GIT_HASH=${TERMINUSDB_GIT_HASH}
ARG TERMINUSDB_JWT_ENABLED=true
ENV TERMINUSDB_JWT_ENABLED=${TERMINUSDB_JWT_ENABLED}
RUN apt-get update && apt-get install -y --no-install-recommends libjwt0 make openssl \
    && rm -rf /var/cache/apt/* && rm -rf /var/lib/apt/lists/* && make $MAKE_ARGS
CMD ["/app/terminusdb/distribution/init_docker.sh"]
