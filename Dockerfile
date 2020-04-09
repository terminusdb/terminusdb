FROM terminusdb/terminus_store_prolog:v0.9.9.1
WORKDIR /usr/lib/swipl/pack
RUN BUILD_DEPS="git build-essential make libjwt-dev libssl-dev" \
        apt-get update && apt-get install -y --no-recommends $BUILD_DEPS \
        && git clone https://github.com/terminusdb/jwt_io.git \
        && cd jwt_io && make && apt-get purge -y $BUILD_DEPS \
        && rm -rf /var/cache/apt/*

FROM terminusdb/terminus_store_prolog:v0.9.9.1
WORKDIR /app/terminusdb
COPY ./ /app/terminusdb
COPY --from=0 /usr/lib/swipl/pack/jwt_io /usr/lib/swipl/pack/jwt_io/
COPY --from=0 /usr/lib/swipl/pack/jwt_io.so /app/terminusdb/
RUN apt-get update && apt-get install -y --no-recommends libjwt0 && rm -rf /var/cache/apt/*
CMD /app/terminusdb/init_docker.sh
