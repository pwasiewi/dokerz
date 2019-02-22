docker-codeblocks
==============

[Docker](http://docker.io) image for a private [codeblocks](http://codeblocks.org) [16.01+dfsg-2](https://launchpad.net/ubuntu/+source/codeblocks/16.01+dfsg-2).
#It still lacks some features e.g. debugger says no tty and does not run

Installation
============

To fetch from Docker index:

```
docker pull 42n4/codeblocks
```

To build from source, run the following from the root of this repo:

```
docker build --rm -t 42n4/codeblocks .
```

Usage
=====

docker run --rm -v /tmp:/tmp -v 42n4/codeblocks $DISPLAY 

```
docker run --rm -v /tmp:/tmp -v 42n4/codeblocks $DISPLAY
[ ! -d ~/remote ] && mkdir ~/remote
docker run --rm -v /tmp:/tmp -v /home/guest/remote:/home/guest 42n4/codeblocks  $DISPLAY

```
