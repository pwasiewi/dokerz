# docker-version 18.10-dev
from        ubuntu:cosmic
MAINTAINER Piotr Wasiewicz <42n4na@gmail.com>

ENV DEBIAN_FRONTEND noninteractive

# for polish translations (change to your own language)
run apt-get update && apt-get install -y apt-utils && apt dist-upgrade -y && apt-get install -y software-properties-common curl wget sudo locales && locale-gen pl_PL.UTF-8 && update-locale LANG=pl_PL.UTF-8
env LANG pl_PL.UTF-8

run apt-get install -y libxext-dev libxrender-dev libxtst-dev

# http://freeciv.wikia.com/wiki/Building_Freeciv_from_source
run apt-get install -y freeciv-client-qt freeciv-sound-standard freeciv-client-extras

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
#cmd /usr/games/freeciv-qt
copy    ./docker-entrypoint.sh /
entrypoint ["/docker-entrypoint.sh"]
cmd [":0"]
