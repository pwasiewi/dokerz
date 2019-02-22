docker-codelite
==============

[Docker](http://docker.io) image for [codelite](http://codelite.org) [9.2+dfsg-1](https://launchpad.net/ubuntu/+source/codelite/10.0+dfsg-1).

It still lacks some features e.g. debugger breakpoints do not work!

Installation
============

To fetch from Docker index:

```
docker pull 42n4/codelite
```

To build from source, run the following from the root of this repo:

```
docker build --rm -t 42n4/codelite .
```

Usage
=====

docker run --rm -v /tmp:/tmp -v 42n4/codelite $DISPLAY 

```
docker run --rm -v /tmp:/tmp -v 42n4/codelite $DISPLAY
[ ! -d ~/remote ] && mkdir ~/remote
docker run --rm -v /tmp:/tmp -v /home/guest/remote:/home/guest 42n4/codelite  $DISPLAY

```
