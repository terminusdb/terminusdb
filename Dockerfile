FROM terminusdb/terminus_store_prolog:v0.9.7
WORKDIR /app/terminusdb
CMD apt update -q && apt install libssl-dev libjwt-dev \
    && swipl -g "pack_install('https://github.com/terminusdb/jwt_io', [interactive(false)])." -g halt \
    && cp /usr/lib/swipl/pack/jwt_io/jwt_io.so /app/terminusdb/
COPY ./ /app/terminusdb
CMD /app/terminusdb/init_docker.sh
