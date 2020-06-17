FROM alpine:latest

RUN apk add --no-cache\
      fuse\
      openssl\
      curl\
      libgomp\
      libstdc++\
    && apk add --no-cache --virtual .build-deps\
      python3\
      py3-pip\
      git\
      cmake\
      make\
      gcc\
      g++\
      curl-dev\
      openssl-dev\
      fuse-dev\
    && mkdir -p /usr/src/\
    && ln -s /usr/bin/python3 /usr/bin/python\
    && python -m pip install conan versioneer\
    && git clone https://github.com/cryfs/cryfs.git /usr/src/cryfs\
    && mkdir /usr/src/cryfs/cmake\
    && cd /usr/src/cryfs/cmake\
    && cmake ..\
    && make\
    && make install\
    && apk del .build-deps\
    && cd /\
    && rm /usr/src/ -rf

ENTRYPOINT /bin/sh
