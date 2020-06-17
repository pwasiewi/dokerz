FROM alpine:20200428
#FROM alpine:edge

RUN apk add --no-cache \
      curl \
      fuse \
      openssl \
      libgomp \
      libstdc++ && \
      apk add --no-cache --virtual .build-deps \ 
      git \
      gcc \
      g++ \
      make \
      cmake \
      curl-dev \
      fuse-dev \
      py3-pip \
      python3 \
      openssl-dev && \
    mkdir -p /usr/src/  && \
    ln -s /usr/bin/python3 /usr/bin/python
RUN python -m pip install conan versioneer --ignore-installed six
RUN git clone https://github.com/cryfs/cryfs.git /usr/src/cryfs
RUN mkdir /usr/src/cryfs/cmake  && \
    cd /usr/src/cryfs/cmake && \
    ls -la /usr/src/cryfs && \
    cmake .. && \
    make && \
    make install && \
    apk del .build-deps && \
    cd / && \
    rm /usr/src/ -rf

ENTRYPOINT /bin/sh
