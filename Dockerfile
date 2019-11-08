FROM swipl:stable
WORKDIR /app
COPY ./ /app/terminusdb
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
        git \
	build-essential \
        curl \
    make
# Install Rust
RUN curl https://sh.rustup.rs -sSf | bash -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN PKG_DIR=/usr/lib/swipl/pack \
	&& echo "pack_remove(terminus_store_prolog).\
                 pack_install(terminus_store_prolog,[package_directory('$PKG_DIR'),interactive(false)]).\
                 pack_install(mavis,[package_directory('$PKG_DIR'),interactive(false)])."  | swipl
CMD /app/terminusdb/init_docker.sh
