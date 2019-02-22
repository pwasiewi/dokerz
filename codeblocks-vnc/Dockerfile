# docker-version 17.06.0-dev
from        ubuntu:zesty
MAINTAINER Piotr Wasiewicz <42n4na@gmail.com>

ENV DEBIAN_FRONTEND noninteractive

# for polish translations (change to your own language)
run apt-get update && apt-get install -y apt-utils && apt-get install -y software-properties-common curl wget sudo locales && locale-gen pl_PL.UTF-8 && update-locale LANG=pl_PL.UTF-8
env LANG pl_PL.UTF-8

run apt-get install -y libxext-dev libxrender-dev libxtst-dev
run apt-get install -y build-essential make gdb

#vnc in lxde
RUN apt-get install -y  --force-yes --no-install-recommends \
        supervisor \
        openssh-server pwgen sudo vim-tiny \
        net-tools \
        lxde x11vnc xvfb \
        nginx \
        python-pip python-dev 
        #mesa-utils libgl1-mesa-dri 
# codeblocks
run apt-get install -y codeblocks codeblocks-common codeblocks-contrib 
run apt-get install -y codeblocks-dbg codeblocks-dev


ENV TINI_VERSION v0.14.0
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini /bin/tini
RUN chmod +x /bin/tini

ADD image /

RUN pip install setuptools wheel && pip install -r /usr/lib/web/requirements.txt

RUN mkdir -p /var/run/sshd

#clean all
RUN apt-get autoclean \
    && apt-get autoremove -y \
    && rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/*

# create an guest user
run export uid=1001 gid=100 PASS=packer && \
    #id -u guest 2>&1 | cat /dev/null || 
    useradd -u $uid -m -g users -G adm,sudo,audio,video guest && \
    #echo "guest:$PASS" | chpasswd && \
    mkdir -p /etc/sudoers.d && \
    echo "guest ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/guest && \
    chmod 0440 /etc/sudoers.d/guest && \
    #mkdir -p /home/ubuntu/.config/pcmanfm/LXDE/ && cp /usr/share/doro-lxde-wallpapers/desktop-items-0.conf /home/ubuntu/.config/pcmanfm/LXDE/ && \
    chown ${uid}:${gid} -R /home/guest 
user guest
env HOME /home/guest

# tini for subreap                                   
EXPOSE 80
#WORKDIR /root
entrypoint ["/docker-entrypoint.sh"]
cmd        []
