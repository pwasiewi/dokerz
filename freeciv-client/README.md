docker-freeciv-client
=====================
Docker image for a freeciv client.

Installation
============
To build from source, run the following from the root of this repo (change to your locale in Dockerfile):

```
     docker build -t 42n4/freeciv-client .
```

Usage
=====
With the following command the client is started in a container and exposed as a window on the host.
This mechanism is described in
http://fabiorehm.com/blog/2014/09/11/running-gui-apps-with-docker/.

```
     xhost +
     docker run --rm -v /tmp/.X11-unix:/tmp/.X11-unix --device /dev/snd 42n4/freeciv-client $DISPLAY
     docker run --rm -v /tmp/.X11-unix:/tmp/.X11-unix --device /dev/snd 42n4/freeciv-client $DISPLAY 192.168.1.12 #with the host ip
     docker run --rm -v /tmp/.X11-unix:/tmp/.X11-unix --device /dev/snd 42n4/freeciv-client $DISPLAY 192.168.1.12 Rex #with the host ip and inside freeciv game user
```

In the game
===========
To connect to a server from within the client
"localhost" has to be replaced by the host server ip.


For [my docker server](https://github.com/pwasiewi/docker-freeciv):

```
docker run -d -v `pwd`/`mktemp -d -p . -t tempXXXX`:/freeciv -p 5556:5556 42n4/freeciv      #start new game in random tempXXX dir
docker run -d -v `pwd`:/freeciv -p 5556:5556 42n4/freeciv freeciv-T0097-Y-0075-auto.sav.bz2 #load game in its directory
```


## License

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png "CC0")](http://creativecommons.org/publicdomain/zero/1.0/deed)

dedicated to public domain, no rights reserved.
