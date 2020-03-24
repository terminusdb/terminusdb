FROM terminusdb/terminus_store_prolog:v0.9.5
WORKDIR /app
COPY ./ /app/terminusdb
CMD /app/terminusdb/init_docker.sh
