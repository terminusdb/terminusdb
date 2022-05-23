FROM terminusdb/swipl:v8.4.2 as pack_installer
RUN set -eux; \
    BUILD_DEPS="git build-essential make libjwt-dev libssl-dev pkg-config"; \
    apt-get update; \
    apt-get install -y --no-install-recommends ${BUILD_DEPS}; \
    rm -rf /var/lib/apt/lists/*
WORKDIR /app/pack
COPY ./Makefile .
RUN set -eux; \
    make install-tus; \
    make install-jwt

FROM terminusdb/swipl:v8.4.2 AS rust_builder
RUN set -eux; \
    BUILD_DEPS="git build-essential curl clang ca-certificates"; \
    apt-get update; \
    apt-get install -y --no-install-recommends ${BUILD_DEPS}; \
    rm -rf /var/lib/apt/lists/*
RUN curl https://sh.rustup.rs -sSf | bash -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
WORKDIR /app/rust
COPY ./src/rust .
RUN cargo build --release

FROM terminusdb/swipl:v8.4.2
RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends libjwt0 make openssl; \
    rm -rf /var/cache/apt/*; \
    rm -rf /var/lib/apt/lists/*
WORKDIR /app/terminusdb
COPY ./ .
COPY --from=pack_installer /root/.local/share/swi-prolog/pack/ /usr/share/swi-prolog/pack
COPY --from=rust_builder /app/rust/target/release/libterminusdb_dylib.so /app/terminusdb/src/rust/librust.so
ARG MAKE_ARGS=""
ARG TERMINUSDB_GIT_HASH=null
ENV TERMINUSDB_GIT_HASH=${TERMINUSDB_GIT_HASH}
ARG TERMINUSDB_JWT_ENABLED=true
ENV TERMINUSDB_JWT_ENABLED=${TERMINUSDB_JWT_ENABLED}
RUN set -eux; \
    touch src/rust/librust.so; \
    make $MAKE_ARGS
CMD ["/app/terminusdb/distribution/init_docker.sh"]
