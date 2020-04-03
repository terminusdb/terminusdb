FROM terminusdb/terminus_store_prolog:v0.9.7
CMD apt update -q && apt install libssl-dev \
    && swipl -g "pack_install('https://github.com/terminusdb/jwt_io', [interactive(false)])." -g halt
WORKDIR /app
COPY ./ /app/terminusdb
CMD /app/terminusdb/init_docker.sh
