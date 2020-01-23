FROM terminusdb/terminus_store_prolog:v0.9.1
WORKDIR /app
COPY ./ /app/terminusdb
CMD /app/terminusdb/init_docker.sh
