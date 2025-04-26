# syntax=docker/dockerfile:1.3

ARG DIST=community

# Install the SWI-Prolog pack dependencies.
FROM swipl:9.2.9 AS pack_installer
RUN set -eux; \
    BUILD_DEPS="git curl build-essential make libjwt-dev libssl-dev pkg-config clang ca-certificates m4 libgmp-dev protobuf-compiler libprotobuf-dev"; \
    apt-get update; \
    apt-get install -y --no-install-recommends ${BUILD_DEPS}; \
    rm -rf /var/lib/apt/lists/*
WORKDIR /app/pack
COPY distribution/Makefile.deps Makefile
RUN make

# Install Rust. Prepare to build the Rust code.
FROM swipl:9.2.9 AS rust_builder_base
ARG CARGO_NET_GIT_FETCH_WITH_CLI=true
RUN set -eux; \
    BUILD_DEPS="git build-essential curl clang ca-certificates m4 libgmp-dev protobuf-compiler libprotobuf-dev"; \
    apt-get update; \
    apt-get install -y --no-install-recommends ${BUILD_DEPS}; \
    rm -rf /var/lib/apt/lists/*
RUN curl https://sh.rustup.rs -sSf | bash -s -- -y --profile minimal
ENV PATH="/root/.cargo/bin:${PATH}"
# Initialize the crates.io index git repo to cache it.
RUN (cargo install lazy_static 2> /dev/null || true) && (cargo install cargo-swipl || true)
WORKDIR /app/rust
COPY distribution/Makefile.rust Makefile
COPY src/rust src/rust/

# Build the community dylib.
FROM rust_builder_base AS rust_builder_community
ARG CARGO_NET_GIT_FETCH_WITH_CLI=true
RUN make DIST=community && cd src/rust && cargo swipl test --release

# Build the enterprise dylib.
FROM rust_builder_base AS rust_builder_enterprise
COPY terminusdb-enterprise/rust terminusdb-enterprise/rust/
RUN make DIST=enterprise

# Build the ${DIST} dylib.
FROM rust_builder_${DIST} AS rust_builder

# Copy the packs and dylib. Prepare to build the Prolog code.
FROM swipl:9.2.9 AS base
RUN set -eux; \
    RUNTIME_DEPS="libjwt0 make openssl binutils ca-certificates"; \
    apt-get update; \
    apt-get upgrade -y; \
    apt-get install -y --no-install-recommends ${RUNTIME_DEPS}; \
    rm -rf /var/cache/apt/*; \
    rm -rf /var/lib/apt/lists/*
ARG TERMINUSDB_GIT_HASH=null
ENV TERMINUSDB_GIT_HASH=${TERMINUSDB_GIT_HASH}
ARG TERMINUSDB_JWT_ENABLED=true
ENV TERMINUSDB_JWT_ENABLED=${TERMINUSDB_JWT_ENABLED}
COPY --from=pack_installer /usr/share/swi-prolog/pack/jwt_io /usr/share/swi-prolog/pack/jwt_io
COPY --from=pack_installer /usr/share/swi-prolog/pack/tus /usr/share/swi-prolog/pack/tus
COPY --from=pack_installer /app/pack/dashboard /app/terminusdb/dashboard
WORKDIR /app/terminusdb
COPY distribution/init_docker.sh distribution/
COPY distribution/Makefile.prolog Makefile
COPY src src/
COPY --from=rust_builder /app/rust/src/rust/librust.so src/rust/

# Build the community executable.
FROM base AS base_community
RUN set -eux; \
    touch src/rust/librust.so; \
    make DIST=community

# Build the enterprise executable.
FROM base AS base_enterprise
COPY terminusdb-enterprise/prolog terminusdb-enterprise/prolog/
RUN set -eux; \
    touch src/rust/librust.so; \
    make DIST=enterprise

# Build the ${DIST} executable. Set the default command.
FROM base_${DIST}
CMD ["/app/terminusdb/distribution/init_docker.sh"]
