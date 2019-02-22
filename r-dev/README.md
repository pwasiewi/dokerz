### For building in a new folder with a Dockerfile in it (you can add your packages to it)

docker build --rm -t 42n4/r-dev .

### Run bash in a docker

docker exec -it r-dev /bin/bash

