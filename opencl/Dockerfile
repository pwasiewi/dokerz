# Using the latest long-term-support Ubuntu OS
FROM ubuntu:16.04

# (Based on https://github.com/pkienzle/opencl_docker and https://gitlab.com/nvidia/opencl)

RUN apt-get update \
 && apt-get install -y apt-utils \
 && apt-get dist-upgrade -y \ 
 && apt-get install -y --no-install-recommends \
    mc \
    vim \ 
    curl \
    wget \
    file \
    unzip \
    alien \
    strace \
    clinfo \
    dirmngr \
    xz-utils \
    lsb-release \
    ca-certificates \
    bash-completion \
    ocl-icd-libopencl1 \
    ocl-icd-opencl-dev \
    apt-transport-https \
    software-properties-common 

# Install AMD Radeon drivers
# sonm/opencl is much smaller since it "pre-installs" the AMD libraries rather
# going through the vendor installer (100 MB vs 800 MB).  Similarly, installing
# alien, etc. so the intel installer can work adds 300+ MB.  Leave it this way
# for now since it should be easier to bump driver versions; also, it should
# be more trustworthy since the docker build relies on vendor binaries rather
# than unsigned object files from an unknown user.
# Docker run command needs --device=/dev/dri (and maybe --device=/dev/kfd)

#https://www.amd.com/en/support/kb/release-notes/rn-prorad-lin-18-40
#ARG AMD_DRIVER=amdgpu-pro-18.10-572953.tar.xz
#ARG AMD_DRIVER=amdgpu-pro-18.30-641594.tar.xz
ARG AMD_DRIVER=amdgpu-pro-18.40-673869-ubuntu-16.04.tar.xz
ARG AMD_DRIVER_URL=https://drivers.amd.com/drivers/linux
RUN mkdir -p /tmp/opencl-driver-amd
WORKDIR /tmp/opencl-driver-amd
RUN echo AMD_DRIVER is $AMD_DRIVER_URL/$AMD_DRIVER; \
    #curl --referer $AMD_DRIVER_URL -O $AMD_DRIVER_URL/$AMD_DRIVER; \
    wget -nv $AMD_DRIVER_URL/$AMD_DRIVER  --referer  http://support.amd.com/en-us/kb-articles/Pages/Radeon-Software-for-Linux-Release-Notes.aspx; \
    tar -Jxvf $AMD_DRIVER; \
    cd amdgpu-pro-*; \
    ./amdgpu-install; \
    apt-get install opencl-amdgpu-pro -y --no-install-recommends; \
    rm -rf /tmp/opencl-driver-amd;

# Install NVidia OpenCL drivers
# In order to run against nvidia GPU need the docker-ce package:
#   https://docs.docker.com/install/linux/docker-ce/ubuntu/#install-docker-ce
# then the nvidia-docker2 package:
#   https://github.com/NVIDIA/nvidia-docker
# The commands to compose the nvidia opencl configuration come from
# the nvidia docker containers in opencl/runtime/Dockerfile at:
#   https://gitlab.com/nvidia/opencl
# These are also hosted on docker hub and can be used as the base container:
#   https://hub.docker.com/r/nvidia/opencl/
# Docker run command needs "--runtime=nvidia"

RUN wget https://www.khronos.org/registry/OpenCL/api/2.1/cl.hpp -O /usr/include/CL/cl.hpp

RUN mkdir -p /etc/OpenCL/vendors && \
    echo "libnvidia-opencl.so.1" > /etc/OpenCL/vendors/nvidia.icd
ENV NVIDIA_VISIBLE_DEVICES all
ENV NVIDIA_DRIVER_CAPABILITIES compute,utility

RUN rm -f /etc/OpenCL/vendors/mesa.icd

CMD clinfo
