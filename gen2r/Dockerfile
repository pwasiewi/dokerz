FROM gentoo/stage3-amd64

RUN mkdir /usr/portage && emerge-webrsync 
RUN echo 'USE="text cups minizip icu lapack nls openmp perl readline"' >> /etc/portage/make.conf && \
    echo 'MAKEOPTS="-j8 -l16"' >> /etc/portage/make.conf && \
    echo 'EMERGE_DEFAULT_OPTS="--jobs 8 --load-average 16 --quiet-build=y --autounmask-write --keep-going --verbose-conflicts --backtrack=3 --autounmask-backtrack=y"' >> /etc/portage/make.conf &&\
    echo 'PORTAGE_NICENESS="19"' >> /etc/portage/make.conf
RUN mkdir /etc/portage/package.keywords && echo "dev-lang/R" >> /etc/portage/package.keywords/moje.keywords

#RUN cat /etc/portage/make.conf
RUN emerge R
