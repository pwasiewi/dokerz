docker-freeciv
==============

[Docker](http://docker.io) image for a private [freeciv](http://freeciv.org) 2.5.6-2ubuntu1 server.

Installation
============

To fetch from Docker index:

```
docker pull 42n4/freeciv
```

To build from source, run the following from the root of this repo:

```
docker build --rm -t 42n4/freeciv .
```

Usage
=====

Freeciv [exposes a server on port 5556](https://docs.docker.com/engine/userguide/networking/default_network/binding/) and uses an [external volume](https://docs.docker.com/docker-cloud/apps/volumes/) for savegame data (/freeciv).

For the simple scenario that you wish to have savegame data stored in /freeciv on the host and map port 5556 in the container to port 5556 on the host, use the command:


```
docker run -d -v `pwd`/`mktemp -d -p . -t tempXXXX`:/freeciv -p 5556:5556 42n4/freeciv #start new game
docker run -d -v `pwd`:/freeciv -p 5556:5556 42n4/freeciv freeciv-T0097-Y-0075-auto.sav.bz2 #load game in its directory
#docker ps -a  					#shows all docker containers
#docker exec -it objective_boyd /bin/bash 	#docker with the name objective_boyd is attached with a shell
#docker rm -f `docker ps -aq` 			#all docker containers are removed
```


Clients should now be able to connect to your freeciv server!

From [my docker client](https://github.com/pwasiewi/docker-freeciv-client):

```
xhost +
docker run --rm -v /tmp/.X11-unix:/tmp/.X11-unix --device /dev/snd 42n4/freeciv-client $DISPLAY
docker run --rm -v /tmp/.X11-unix:/tmp/.X11-unix --device /dev/snd 42n4/freeciv-client $DISPLAY 192.168.1.12 #with the host ip
docker run --rm -v /tmp/.X11-unix:/tmp/.X11-unix --device /dev/snd 42n4/freeciv-client $DISPLAY 192.168.1.12 Rex #with the host ip and inside freeciv game user
```

## License

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png "CC0")](http://creativecommons.org/publicdomain/zero/1.0/deed)

dedicated to public domain, no rights reserved.

