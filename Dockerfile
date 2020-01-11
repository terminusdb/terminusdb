FROM terminusdb/terminus_store_prolog:v0.8.1
WORKDIR /app
COPY ./ /app/terminusdb
CMD /app/terminusdb/init_docker.sh
