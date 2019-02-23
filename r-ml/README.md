M$ RevoScaleR Docker
======================

### I have added M$ R and support to M$ RevoScaleR for packages compilation and some shell commands from littler

#### (Based on https://github.com/rocker-org/rocker)

[![Docker Pulls](https://img.shields.io/docker/pulls/42n4/r-ml.svg)](https://hub.docker.com/r/42n4/r-ml/)
[![Docker Stars](https://img.shields.io/docker/stars/42n4/r-ml.svg)](https://hub.docker.com/r/42n4/r-ml/)

### You can pull my docker
docker pull 42n4/r-ml   #pull my docker (about 7G in tar)

### or make it - you can add your packages to Dockerfile:
git clone https://github.com/pwasiewi/dokerz.git

cd dokerz/r-ml

docker build --rm -t 42n4/r-ml .

### Login
docker login 				#docker-hub-user-login and pass to hub.docker.com

docker push 42n4/r-ml 	#send to docker-hub-user/docker-name

### Docker offline copy
docker save 42n4/r-ml > ~/docker42n4r-ml.tar 

docker load < ~/docker42n4r-ml.tar

### Run R in a docker
docker run -it 42n4/r-ml R

### Run bash in a active docker
docker exec -it r-ml /bin/bash
