FROM 42n4/r-ml:latest

## (Based on https://github.com/rocker-org/rocker/blob/master/rstudio/3.4.3/Dockerfile)

ARG RSTUDIO_VERSION
ARG PANDOC_TEMPLATES_VERSION 
ENV PANDOC_TEMPLATES_VERSION ${PANDOC_TEMPLATES_VERSION:-2.1.1}

## Add RStudio binaries to PATH
ENV PATH /usr/lib/rstudio-server/bin:$PATH

RUN apt-get update && apt-get install -y --no-install-recommends git libcurl4-openssl-dev libedit2 psmisc python-setuptools sudo libgstreamer0.10-0 libgstreamer-plugins-base0.10-0 libclang-dev
     
RUN RSTUDIO_LATEST=$(wget --no-check-certificate -qO- https://s3.amazonaws.com/rstudio-server/current.ver) \
  && [ -z "$RSTUDIO_VERSION" ] && RSTUDIO_VERSION=$RSTUDIO_LATEST || true \
  && wget -q http://download2.rstudio.org/rstudio-server-${RSTUDIO_VERSION}-amd64.deb \
  && dpkg -i rstudio-server-${RSTUDIO_VERSION}-amd64.deb \
  #&& wget -q https://s3.amazonaws.com/rstudio-ide-build/server/trusty/amd64/rstudio-server-1.2.1280-amd64.deb \
  #&& dpkg -i rstudio-server-1.2.1280-amd64.deb \
  && rm rstudio-*-amd64.deb \
  ## Symlink pandoc & standard pandoc templates for use system-wide
  && ln -s /usr/lib/rstudio-server/bin/pandoc/pandoc /usr/local/bin \
  && ln -s /usr/lib/rstudio-server/bin/pandoc/pandoc-citeproc /usr/local/bin \
  && wget https://github.com/jgm/pandoc-templates/archive/${PANDOC_TEMPLATES_VERSION}.tar.gz \
  && mkdir -p /opt/pandoc/templates && tar zxf ${PANDOC_TEMPLATES_VERSION}.tar.gz \
  && cp -r pandoc-templates*/* /opt/pandoc/templates && rm -rf pandoc-templates* \
  && mkdir /root/.pandoc && ln -s /opt/pandoc/templates /root/.pandoc/templates \
  && rm ${PANDOC_TEMPLATES_VERSION}.tar.gz \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  ## RStudio wants an /etc/R, will populate from $R_HOME/etc
  && mkdir -p /etc/R \
  ## Write config files in $R_HOME/etc
  && echo " " >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Rprofile.site \
  && echo "# Configure httr to perform out-of-band authentication if HTTR_LOCALHOST" >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Rprofile.site \
  && echo "# is not set since a redirect to localhost may not work depending upon" >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Rprofile.site \
  && echo "# where this Docker container is running." >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Rprofile.site \
  && echo "if(is.na(Sys.getenv(\"HTTR_LOCALHOST\", unset=NA))) {" >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Rprofile.site \
  && echo "  options(httr_oob_default = TRUE)" >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Rprofile.site \
  && echo "}" >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Rprofile.site \
  && echo "PATH=\"${PATH}\"" >> /opt/microsoft/mlserver/9.3.0/runtime/R/etc/Renviron \
  ## configure git not to request password each time
  && git config --system credential.helper 'cache --timeout=3600' \
  && git config --system push.default simple \
  ## Set up S6 init system
  && wget -P /tmp/ https://github.com/just-containers/s6-overlay/releases/download/v1.21.8.0/s6-overlay-amd64.tar.gz \
  && tar xzf /tmp/s6-overlay-amd64.tar.gz -C / \
  && mkdir -p /etc/services.d/rstudio \
  && echo '#!/bin/bash ' > /etc/services.d/rstudio/run \
  && echo 'exec /usr/lib/rstudio-server/bin/rserver --server-daemonize 0' >> /etc/services.d/rstudio/run \
  && echo '#!/bin/bash' > /etc/services.d/rstudio/finish \
  && echo 'rstudio-server stop' >> /etc/services.d/rstudio/finish 

RUN echo "rsession-which-r=/opt/microsoft/mlserver/9.3.0/runtime/R" >> /etc/rstudio/rserver.conf \
  && echo "rsession-ld-library-path=/opt/microsoft/mlserver/9.3.0/runtime/R/library:/opt/microsoft/mlserver/9.3.0/libraries/RServer" >> /etc/rstudio/rserver.conf

COPY userconf.sh /etc/cont-init.d/userconf

## running with "-e ADD=shiny" adds shiny server
COPY add_shiny.sh /etc/cont-init.d/add

EXPOSE 8787
EXPOSE 3838

## automatically link a shared volume for kitematic users
#VOLUME /home/rstudio/kitematic

CMD ["/init"]

