# docker-version 17.06.0-dev
from        ubuntu:zesty
MAINTAINER Piotr Wasiewicz <42n4na@gmail.com>

ENV DEBIAN_FRONTEND noninteractive

# for polish translations (change to your own language)
run apt-get update && apt-get install -y apt-utils && apt-get install -y software-properties-common curl wget sudo locales && locale-gen pl_PL.UTF-8 && update-locale LANG=pl_PL.UTF-8
env LANG pl_PL.UTF-8

run apt-get install -y libxext-dev libxrender-dev libxtst-dev
run apt-get install -y build-essential make gdb
run apt-get install -y codelite
#clean all
run apt-get clean && apt-get -y autoremove &&\
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/*

# create a user
run export uid=1001 gid=100 && \
    useradd -u $uid -m -g users -G audio guest && \
    mkdir -p /etc/sudoers.d && \
    echo "guest ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/guest && \
    chmod 0440 /etc/sudoers.d/guest && \
    chown ${uid}:${gid} -R /home/guest
user guest
env HOME /home/guest

# run the client
copy    ./docker-entrypoint.sh /
entrypoint ["/docker-entrypoint.sh"]
cmd [":0"]
