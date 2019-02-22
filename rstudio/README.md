## M$ RevoScaleR Rstudio Docker with libraries and scripts of Cichosz book: https://www.amazon.com/Data-Mining-Algorithms-Explained-Using/dp/111833258X and my lectures of data mining
#### Based on (https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image)
### You can pull my docker with all R packages (about 1h with a good internet connection)

docker pull 42n4/rstudio   #pull my docker (about 7G in tar)

git clone https://github.com/pwasiewi/dokerz.git

### or make it in three steps (about 1 hour on i7) - you can add your packages to Dockerfile:
#### 1
cd dokerz/r-ml

docker build --rm -t 42n4/r-ml .
#### 2
cd ../rstudio-ml

docker build --rm -t 42n4/rstudio-ml .
#### 3
cd ../rstudio

docker build --rm -t 42n4/rstudio .

docker login 				#docker-hub-user-login and pass to hub.docker.com

docker push 42n4/rstudio 	#send to docker-hub-user/docker-name

### docker offline copy
docker save 42n4/rstudio > ~/docker42n4.tar 

docker load < ~/docker42n4.tar

### Install 7zip. Unpack all data in a directory data with 8x from a directory bin. 
#### execute in a directory data: 

cd data; 

sh AirOnTime87to12.xdf.sh 

../bin/8x census-income.tar.7z

../bin/8x covandcommunities.tar.7z

../bin/8x retailchurn.tar.7z

cd ..

## Start the docker in Linux with exact username and its user id and group id (login: guest pass: rstudio)

docker run -d -p 8787:8787 --name=rstudio -e USER=guest -e USERID=$(id -u) -e GROUPID=$(id -g) -v $(pwd):/home/guest 42n4/rstudio

### Start the docker in MSWindows (Docker for Windows) with Linux containers enabled and Powershell and shared disk c: in docker settings (login: rstudio pass: rstudio)
https://github.com/pwasiewi/dokerz/blob/master/rstudio/linux_docker_in_windows10.png

docker run -d -p 8787:8787 --name=rstudio --restart=always -v c:/Users/Piotr/remote:/home/rstudio 42n4/rstudio

### To turn off hyper-v in powershell after removing docker for windows in order to use e.g. Virtualbox, Vmware

Disable-WindowsOptionalFeature -Online -FeatureName Microsoft-Hyper-V-All
### or
dism.exe /Online /Disable-Feature:Microsoft-Hyper-V-All

### To turn on hyper-v in powershell
dism.exe /Online /Enable-Feature:Microsoft-Hyper-V /All

### In order to use VirtualBox with Docker use Docker ToolBox: https://www.docker.com/products/docker-toolbox

### Docker for Windows always use hyper-v disabling other vm providers

##### https://forums.docker.com/t/volume-mounts-in-windows-does-not-work/10693/141
##### https://coderwall.com/p/2rpbba/docker-create-a-bridge-and-shared-network

### Run bash in the docker
docker run -it 42n4/rstudio /bin/bash

### Run bash in the active docker
docker exec -it rstudio /bin/bash

### Get or update the docker
docker pull 42n4/rstudio

### Stop and remove the running docker
docker rm -f rstudio

### or just use
bin/docker_remove

### After building a docker clean your hd from unwanted temporary image layers
bin/docker_clean

