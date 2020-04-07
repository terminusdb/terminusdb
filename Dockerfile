FROM terminusdb/terminus_store_prolog:v0.9.9
WORKDIR /app/terminusdb
COPY ./ /app/terminusdb
CMD /app/terminusdb/init_docker.sh
