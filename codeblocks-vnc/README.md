docker-codeblocks-vnc
=========================

[![Docker Pulls](https://img.shields.io/docker/pulls/42n4/codeblocks-vnc.svg)](https://hub.docker.com/r/42n4/codeblocks-vnc/)
[![Docker Stars](https://img.shields.io/docker/stars/42n4/codeblocks-vnc.svg)](https://hub.docker.com/r/42n4/codeblocks-vnc/)

From Docker Index
```
docker pull 42n4/codeblocks-vnc
```

Build yourself
```
git clone https://github.com/42n4/dockerz.git && cd dockerz/codeblocks-vnc
docker build --rm -t 42n4/codeblocks-vnc .
```

Run
```
[ ! -d ~/remote ] && mkdir ~/remote
docker run -it --rm -v ~/remote:/home/guest -p 6080:80 42n4/codeblocks-vnc
```

Browse http://127.0.0.1:6080/

[//]: # (<img src="https://raw.github.com/42n4/docker-ubuntu-vnc-desktop/master/screenshots/lxde.png" width=400/>)


License
==================

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png "CC0")](http://creativecommons.org/publicdomain/zero/1.0/deed)

dedicated to public domain, no rights reserved.

