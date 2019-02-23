M$ RevoScaleR RStudio Docker
======================

### I have added M$ RevoScaleR for RStudio

#### (Based on https://github.com/rocker-org/rocker)

[![Docker Pulls](https://img.shields.io/docker/pulls/42n4/rstudio-ml.svg)](https://hub.docker.com/r/42n4/rstudio-ml/)
[![Docker Stars](https://img.shields.io/docker/stars/42n4/rstudio-ml.svg)](https://hub.docker.com/r/42n4/rstudio-ml/)

### You can pull my docker

docker pull 42n4/rstudio-ml   #pull my docker (about 7G in tar)

### or make it in two steps (about a half of hour on i7) - you can add your packages to Dockerfile as I have done in https://github.com/pwasiewi/dokerz/tree/master/rstudio:
#### 1
git clone https://github.com/pwasiewi/dokerz.git

cd dokerz/r-ml

docker build --rm -t 42n4/r-ml .
#### 2
cd ../rstudio-ml

docker build --rm -t 42n4/rstudio-ml .

### Login
docker login 				#docker-hub-user-login and pass to hub.docker.com

docker push 42n4/rstudio 	#send to docker-hub-user/docker-name

### Start the docker in Linux with exact username and its user id and group id (login: guest pass: rstudio)
docker run -d -p 8787:8787 --name=rstudio-ml -e USER=guest -e USERID=$(id -u) -e GROUPID=$(id -g) -v $(pwd):/home/guest 42n4/rstudio-ml

### Docker offline copy
docker save 42n4/rstudio > ~/docker42n4.tar 

docker load < ~/docker42n4.tar

### Run bash in a docker

docker run -it 42n4/r-ml /bin/bash

### Run bash in an active docker

docker exec -it r-ml /bin/bash

