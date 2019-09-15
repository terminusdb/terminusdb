FROM swipl:stable
WORKDIR /app
COPY ./ /app/terminusdb
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
	autoconf \
	libtool \
	zlib1g zlib1g-dev \
	pkg-config \
	autotools-dev \
	g++ \
	python \
	git \
	automake \
    make \
    raptor2-utils
RUN cd /app \
        && git clone --branch v1.3.3 https://github.com/rdfhdt/hdt-cpp.git \
        && git clone --branch v0.30.0 https://github.com/drobilla/serd.git \
        && cd /app/serd && ./waf configure && ./waf && ./waf install && \
	cd /app/hdt-cpp && ./autogen.sh && ./configure && make -j4 && make install && cd / \
	&& ldconfig \
	&& PKG_DIR=/usr/lib/swipl/pack \
	&& echo "pack_remove('hdt').\
                 pack_install('https://github.com/GavinMendelGleason/hdt.git',[package_directory('$PKG_DIR'),interactive(false)]).\
                 pack_install(mavis,[package_directory('$PKG_DIR'),interactive(false)])."  | swipl
CMD /app/terminusdb/init_docker.sh
