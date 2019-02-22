FROM ubuntu:16.04

## (Based on https://github.com/rocker-org/rocker/blob/master/r-base/Dockerfile)

# reserve accounts ids 1000 1001
RUN useradd rstudio \
  && echo "rstudio:rstudio" | chpasswd \
  && mkdir /home/rstudio \
  && addgroup rstudio staff \
  && chown rstudio:rstudio /home/rstudio \
  && useradd guest \
  && echo "guest:rstudio" | chpasswd \
  && mkdir /home/guest \
  && addgroup guest staff \
  && chown guest:staff /home/guest 

#https://blogs.msdn.microsoft.com/mlserver/2018/07/26/dockerizing-r-and-python-web-services/
RUN apt-get -y update \
    && apt-get install -y apt-utils \
    && apt-get install -y apt-transport-https lsb-release software-properties-common gnupg dirmngr ca-certificates file bash-completion wget vim mc \
    && apt-get dist-upgrade -y \
    && echo "deb [arch=amd64] https://packages.microsoft.com/repos/azure-cli/ xenial main" | tee /etc/apt/sources.list.d/azure-cli.list \
    && wget https://packages.microsoft.com/config/ubuntu/16.04/packages-microsoft-prod.deb -O /tmp/prod.deb \
    && dpkg -i /tmp/prod.deb \
    && rm -f /tmp/prod.deb \
    && apt-key adv --keyserver packages.microsoft.com --recv-keys 52E16F86FEE04B979B07E28DB02C46DF417A0893 \
    && apt-get -y update \
    && apt-get install -y microsoft-r-open-foreachiterators-3.4.3 \
    && apt-get install -y microsoft-r-open-mkl-3.4.3 \
    && apt-get install -y microsoft-r-open-mro-3.4.3 \
    && apt-get install -y microsoft-mlserver-packages-r-9.3.0 \
#    && apt-get install -y microsoft-mlserver-python-9.3.0 \
#    && apt-get install -y microsoft-mlserver-packages-py-9.3.0 \
    && apt-get install -y microsoft-mlserver-mml-r-9.3.0 \
#    && apt-get install -y microsoft-mlserver-mml-py-9.3.0 \
    && apt-get install -y microsoft-mlserver-mlm-r-9.3.0 \
#    && apt-get install -y microsoft-mlserver-mlm-py-9.3.0 \
    && apt-get install -y azure-cli=2.0.26-1~xenial \
    && apt-get install -y dotnet-runtime-2.0.0 \
    && apt-get install -y microsoft-mlserver-adminutil-9.3.0 \
    && apt-get install -y microsoft-mlserver-config-rserve-9.3.0 \
    && apt-get install -y microsoft-mlserver-computenode-9.3.0 \
    && apt-get install -y microsoft-mlserver-webnode-9.3.0 \
    && apt-get clean \
&& /opt/microsoft/mlserver/9.3.0/bin/R/activate.sh

RUN apt-get install -y build-essential zlib1g-dev libgit2-dev libssl-dev
#    && apt-get purge locales \
#    && apt-get install locales -y \
#    && dpkg-reconfigure locales \
#    && echo "pl_PL.UTF-8 UTF-8" >> /etc/locale.gen \
#    && locale-gen pl_PL.utf8 \
#    && /usr/sbin/update-locale LANG=pl_PL.UTF-8 

  ## install packages from date-locked MRAN snapshot of CRAN
RUN [ -z "$BUILD_DATE" ] && BUILD_DATE=$(TZ="Europe/Warsaw" date -I) || true \
  && MRAN=https://mran.microsoft.com/snapshot/${BUILD_DATE} \
  && echo MRAN=$MRAN >> /etc/environment \
  && export MRAN=$MRAN \
  && echo "options(repos = c(CRAN='$MRAN'))" >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Rprofile.site \
  && ln -sfn /bin/sed /usr/bin/sed \
  ## Fix library path
  && chown root:staff /opt/microsoft/mlserver/9.3.0/libraries/RServer \
  && chmod g+wx /opt/microsoft/mlserver/9.3.0/libraries/RServer \
  && echo "R_LIBS_USER='/opt/microsoft/mlserver/9.3.0/libraries/RServer'" >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Renviron \
  && mkdir /usr/local/lib/R \
  && ln -sfn /opt/microsoft/mlserver/9.3.0/libraries/RServer /usr/local/lib/R/site-library \
  && mv /opt/microsoft/ropen/3.4.3/lib64/R/library/* /opt/microsoft/mlserver/9.3.0/libraries/RServer \
  && rm -rf /opt/microsoft/ropen/3.4.3/lib64/R/library \
  && ln -sfn /opt/microsoft/mlserver/9.3.0/libraries/RServer /opt/microsoft/ropen/3.4.3/lib64/R/library \
  && rm -rf /opt/microsoft/mlserver/9.3.0/runtime/R/library \
  && ln -sfn /opt/microsoft/mlserver/9.3.0/libraries/RServer /opt/microsoft/mlserver/9.3.0/runtime/R/library \
  #RUN echo 'R_LIBS_SITE=/opt/microsoft/mlserver/9.3.0/libraries/RServer:/opt/microsoft/mlserver/9.3.0/libraries/RServer' >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Renviron
  ## for MRAN installation scripts
  && sed -i 's/^LIBR\ =\ /LIBR\ =\ \-L\$\(R_HOME\)\/lib\$\(R_ARCH\)\ -lR/g' /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Makeconf \
  && sed -i '/^LIBS\ =/d' /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Makeconf 
  ##Microsoft key
RUN echo "CURL_CA_BUNDLE='/opt/microsoft/mlserver/9.3.0/runtime/R/lib/microsoft-r-cacert.pem'" >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Renviron \
  && echo 'Sys.setenv(CURL_CA_BUNDLE = file.path(Sys.getenv("R_HOME"), "lib/microsoft-r-cacert.pem"))' >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Rprofile.site 

RUN Rscript -e "install.packages(c('devtools','shiny'), repo = 'https://cran.rstudio.com/')" 
RUN Rscript -e "install.packages(c('littler', 'docopt'))" \
  && ln -s /opt/microsoft/mlserver/9.3.0/libraries/RServer/littler/examples/install2.r /usr/local/bin/install2.r \
  && ln -s /opt/microsoft/mlserver/9.3.0/libraries/RServer/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
  && ln -s /opt/microsoft/mlserver/9.3.0/libraries/RServer/littler/bin/r /usr/local/bin/r \
  && echo "/opt/microsoft/ropen/3.4.3/lib64/R/lib/" >> /etc/ld.so.conf.d/libc.conf \
  && ldconfig
RUN install2.r curl httr remotes
RUN apt-get install -y libpython2.7
RUN install2.r reticulate
RUN userdel guest && rmdir /home/guest

  ## Clean up from R source install
RUN cd / \
  && rm -rf /tmp/* \
  && apt-get autoremove -y \
  && apt-get autoclean -y \
  && rm -rf /var/lib/apt/lists/*

CMD ["R"]

