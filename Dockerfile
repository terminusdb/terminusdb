FROM terminusdb/terminus_store_prolog:v0.10.1
WORKDIR /usr/lib/swipl/pack
RUN export BUILD_DEPS="git build-essential make libjwt-dev libssl-dev pkg-config" \
        && apt-get update && apt-get install $BUILD_DEPS -y --no-install-recommends \
        && git clone https://github.com/terminusdb/jwt_io.git \
        && cd jwt_io && make

FROM terminusdb/terminus_store_prolog:v0.10.1
WORKDIR /app/terminusdb
COPY ./ /app/terminusdb
COPY --from=0 /usr/lib/swipl/pack/jwt_io /usr/lib/swipl/pack/jwt_io/
RUN cp /usr/lib/swipl/pack/jwt_io/jwt_io.so . \
        && apt-get update && apt-get install -y --no-install-recommends libjwt0 \
        && rm -rf /var/cache/apt/* && rm -rf /var/lib/apt/lists/*
CMD /app/terminusdb/init_docker.sh
