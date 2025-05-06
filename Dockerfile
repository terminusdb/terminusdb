# syntax=docker/dockerfile:1.3

ARG DIST=community

FROM debian:bookworm-slim AS swipl_minimal

# Set environment to avoid interactive prompts during installation
ENV DEBIAN_FRONTEND=noninteractive

# Install only essential dependencies for running SWI-Prolog
RUN apt-get update && \
    apt-get install -y \
    swi-prolog \
    libgmp10 \
    libarchive13 \
    libreadline8 \
    zlib1g \
    libjwt0 \
    libssl3 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install the SWI-Prolog pack dependencies.
#FROM swipl:9.2.9 AS pack_installer
FROM swipl_minimal AS pack_installer
RUN set -eux; \
    BUILD_DEPS="git curl build-essential make libjwt-dev libssl-dev pkg-config clang ca-certificates m4 libgmp-dev protobuf-compiler libprotobuf-dev"; \
    apt-get update; \
    apt-get install -y --no-install-recommends ${BUILD_DEPS}; \
    rm -rf /var/lib/apt/lists/*
WORKDIR /app/pack
COPY distribution/Makefile.deps Makefile
RUN make

# Install Rust. Prepare to build the Rust code.
#FROM swipl:9.2.9 AS rust_builder_base
FROM swipl_minimal AS rust_builder_base
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
FROM pack_installer AS base
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
#COPY --from=pack_installer /usr/share/swi-prolog/pack/jwt_io /usr/share/swi-prolog/pack/jwt_io
#COPY --from=pack_installer /usr/share/swi-prolog/pack/tus /usr/share/swi-prolog/pack/tus
WORKDIR /app/terminusdb
COPY distribution/init_docker.sh distribution/
COPY distribution/Makefile.prolog Makefile
COPY src src/
COPY --from=rust_builder /app/rust/src/rust/librust.so src/rust/

# Build the community executable.
FROM base AS base_community
COPY --from=rust_builder /app/rust/src/rust/librust.so src/rust/
RUN set -eux; \
    make DIST=community

# Build the enterprise executable.
FROM base AS base_enterprise
COPY --from=rust_builder /app/rust/src/rust/librust.so src/rust/
COPY terminusdb-enterprise/prolog terminusdb-enterprise/prolog/
RUN set -eux; \
    make DIST=enterprise
    
FROM swipl_minimal AS min_community
COPY --from=base_community /app/terminusdb/terminusdb app/terminusdb/
COPY --from=base /app/terminusdb/distribution/init_docker.sh app/terminusdb/distribution/
CMD ["/app/terminusdb/distribution/init_docker.sh"]

FROM swipl_minimal AS min_enterprise
COPY --from=base_enterprise /app/terminusdb/terminusdb app/terminusdb/
COPY --from=base /app/terminusdb/distribution/init_docker.sh app/terminusdb/distribution/
CMD ["/app/terminusdb/distribution/init_docker.sh"]

# Build the ${DIST} executable. Set the default command.
FROM min_${DIST}
