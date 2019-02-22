# docker-version 17.06.0-dev
from        ubuntu:zesty
MAINTAINER Piotr Wasiewicz <42n4na@gmail.com>

ENV DEBIAN_FRONTEND noninteractive

# for polish translations (change to your own language)
run apt-get update && apt-get install -y apt-utils software-properties-common curl wget sudo && apt-get install -y locales && locale-gen pl_PL.UTF-8 && update-locale LANG=pl_PL.UTF-8
env LANG pl_PL.UTF-8

run apt-get install -y libxext-dev libxrender-dev libxtst-dev

RUN sed 's/main$/main universe/' -i /etc/apt/sources.list && \
    add-apt-repository ppa:webupd8team/java -y && \
    apt-get update && \
    echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections && \
    apt-get install -y oracle-java8-installer 

# Install libgtk as a separate step so that we can share the layer above with
# the netbeans image
RUN apt-get install -y libgtk2.0-0 libcanberra-gtk-module

RUN apt-get install -y build-essential make gdb

#clean all
RUN apt-get autoclean \
    && apt-get autoremove -y \
    && rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/*


#RUN wget http://mirror.onet.pl/pub/mirrors/eclipse//technology/epp/downloads/release/neon/3/eclipse-java-neon-3-linux-gtk-x86_64.tar.gz -O /tmp/eclipse.tar.gz -q && \
RUN wget http://mirror.onet.pl/pub/mirrors/eclipse//technology/epp/downloads/release/neon/3/eclipse-cpp-neon-3-linux-gtk-x86_64.tar.gz -O /tmp/eclipse.tar.gz -q && \
    echo 'Installing eclipse' && \
    tar -xf /tmp/eclipse.tar.gz -C /opt && \
    rm /tmp/eclipse.tar.gz

ADD run /usr/local/bin/eclipse
run chmod 755 /usr/local/bin/eclipse
run chmod +s /usr/bin/gdb

# create a user
run export uid=1001 gid=100 && \
    useradd -u $uid -m -g users -G sudo,adm,audio,video guest && \
    mkdir -p /etc/sudoers.d && \
    echo "guest ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/guest && \
    chmod 0440 /etc/sudoers.d/guest && \
    chown ${uid}:${gid} -R /home/guest
user guest
env HOME /home/guest

WORKDIR /home/guest
CMD /usr/local/bin/eclipse
