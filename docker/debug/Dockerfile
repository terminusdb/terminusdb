# syntax=docker/dockerfile:1.3
# Debug Dockerfile - builds SWI-Prolog from source with debug flags

ARG DIST=community
ARG SKIP_TESTS=false
ARG SWIPL_VERSION=9.2.9

# Build SWI-Prolog from source with debug flags
FROM debian:bookworm AS swipl_debug_builder
ARG SWIPL_VERSION

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential cmake ninja-build git \
    libgmp-dev libssl-dev zlib1g-dev libreadline-dev \
    libedit-dev libarchive-dev libossp-uuid-dev \
    libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
    libxpm-dev libxt-dev pkg-config libdb-dev libpcre2-dev libyaml-dev \
    curl ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build
RUN curl -L https://www.swi-prolog.org/download/stable/src/swipl-${SWIPL_VERSION}.tar.gz | tar xz

WORKDIR /build/swipl-${SWIPL_VERSION}/build
# Build with Debug flags - enables assertions and extra checks
RUN cmake .. \
    -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_INSTALL_PREFIX=/usr \
    -DSWIPL_PACKAGES_JAVA=OFF \
    -DSWIPL_PACKAGES_X=OFF \
    -DINSTALL_DOCUMENTATION=OFF \
    -G Ninja
RUN ninja && ninja install

# Debug SWI-Prolog base image
FROM debian:bookworm-slim AS swipl_debug
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgmp10 libssl3 zlib1g libreadline8 libedit2 libarchive13 \
    libossp-uuid16 libpcre2-8-0 libyaml-0-2 libjwt0 \
    && rm -rf /var/lib/apt/lists/*
COPY --from=swipl_debug_builder /usr/lib/swipl /usr/lib/swipl
COPY --from=swipl_debug_builder /usr/bin/swipl* /usr/bin/

# Verify debug build and chk_secure capability
RUN swipl --version && \
    swipl -g "current_prolog_flag(debug, X), format('Debug flag: ~w~n', [X]), halt" && \
    echo "Testing chk_secure..." && \
    swipl -d chk_secure -g "prolog_debug(chk_secure), format('chk_secure is ACTIVE~n'), halt" && \
    echo "✅ Debug SWI-Prolog with chk_secure verified"

# Install the SWI-Prolog pack dependencies.
FROM swipl_debug AS pack_installer
RUN set -eux; \
    BUILD_DEPS="git curl build-essential make libjwt-dev libssl-dev pkg-config clang ca-certificates m4 libgmp-dev protobuf-compiler libprotobuf-dev"; \
    apt-get update; \
    apt-get install -y --no-install-recommends ${BUILD_DEPS}; \
    rm -rf /var/lib/apt/lists/*
WORKDIR /app/pack
COPY distribution/Makefile.deps Makefile
RUN make

# Install Rust. Prepare to build the Rust code.
FROM swipl_debug AS rust_builder_base
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
ARG SKIP_TESTS=false
RUN make DIST=community && ([ "$SKIP_TESTS" = "true" ] || (cd src/rust && cargo swipl test --release))

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
WORKDIR /app/terminusdb
COPY distribution/init_docker.sh distribution/
COPY distribution/Makefile.prolog Makefile
COPY src src/
COPY --from=rust_builder /app/rust/src/rust/librust.so src/rust/

# Build the community executable.
FROM base AS base_community
ARG SKIP_TESTS=false
COPY --from=rust_builder /app/rust/src/rust/librust.so src/rust/
RUN set -eux; \
    make DIST=community; \
    [ "$SKIP_TESTS" = "true" ] || make test

# Build the enterprise executable.
FROM base AS base_enterprise
ARG SKIP_TESTS=false
COPY --from=rust_builder /app/rust/src/rust/librust.so src/rust/
COPY terminusdb-enterprise/prolog terminusdb-enterprise/prolog/
RUN set -eux; \
    make DIST=enterprise; \
    [ "$SKIP_TESTS" = "true" ] || make test
    
FROM swipl_debug AS min_community
COPY --from=base_community /app/terminusdb/terminusdb app/terminusdb/

FROM swipl_debug AS min_enterprise
COPY --from=base_enterprise /app/terminusdb/terminusdb app/terminusdb/

# Build the ${DIST} executable. Set the default command.
FROM min_${DIST}
COPY --from=base /app/terminusdb/distribution/init_docker.sh app/terminusdb/
RUN set -eux; \
    RUNTIME_DEPS="libjwt0 make openssl binutils ca-certificates"; \
    apt-get update; \
    apt-get upgrade -y; \
    apt-get install -y --no-install-recommends ${RUNTIME_DEPS}; \
    rm -rf /var/cache/apt/*; \
    rm -rf /var/lib/apt/lists/*
# Packs are installed to user's home when using pack_install
COPY --from=pack_installer /root/.local/share/swi-prolog/pack/jwt_io /usr/lib/swipl/pack/jwt_io
COPY --from=pack_installer /root/.local/share/swi-prolog/pack/tus /usr/lib/swipl/pack/tus

WORKDIR /app/terminusdb
RUN mkdir -p storage \
    && chown -R 1000:1000 storage
ENV TERMINUSDB_PLUGINS_PATH=${TERMINUSDB_PLUGINS_PATH:-/plugins}
COPY docker/plugins/auto-optimize.pl ${TERMINUSDB_PLUGINS_PATH}/
RUN mkdir -p /app/terminusdb/dashboard/assets
COPY dashboard/src/index.html /app/terminusdb/dashboard/
COPY dashboard/src/output.css /app/terminusdb/dashboard/assets/

# Copy debug init script and enable chk_secure
COPY docker/debug/init_docker_debug.sh /app/terminusdb/init_docker_debug.sh
RUN chmod +x /app/terminusdb/init_docker_debug.sh

# Enable chk_secure debugging by default in debug builds
# This is checked at runtime in src/start.pl
ENV TERMINUSDB_CHK_SECURE=true

CMD ["/app/terminusdb/init_docker_debug.sh"]
