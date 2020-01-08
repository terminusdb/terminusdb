FROM terminusdb/terminus_store_prolog:v0.7.7
WORKDIR /app
COPY ./ /app/terminusdb
CMD /app/terminusdb/init_docker.sh
