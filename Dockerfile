FROM terminusdb/terminus_store_prolog:v0.9.4
WORKDIR /app
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       curl
COPY ./ /app/terminusdb
CMD /app/terminusdb/init_docker.sh
