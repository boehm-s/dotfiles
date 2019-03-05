FROM ubuntu:18.04

MAINTAINER boehm_s <boehm_s@etna-alternance.net>

RUN export DEBIAN_FRONTEND=noninteractive


# basic stuff
RUN echo 'APT::Get::Assume-Yes "true";' >> /etc/apt/apt.conf \
    && apt-get update && apt-get install -yq \
    bash \
    build-essential \
    dbus-x11 \
    fontconfig \
    git \
    gzip \
    language-pack-en-base \
    libgl1-mesa-glx \
    make \
    sudo \
    tar \
    unzip \
# su-exec
    && git clone https://github.com/ncopa/su-exec.git /tmp/su-exec \
    && cd /tmp/su-exec \
    && make \
    && chmod 770 su-exec \
    && mv ./su-exec /usr/local/sbin/ \
# Cleanup
    && apt-get purge build-essential \
    && apt-get autoremove \
    && rm -rf /tmp/* /var/lib/apt/lists/* /root/.cache/*


# emacs dep

RUN apt-get update && apt-get install -yq \
    libxaw3dxft8 \
    libxaw3dxft8-dev \
    libcairo2 \
    libcairo2-dev \
    librsvg2-common \
    librsvg2-bin \
    librsvg2-2 \
    liblcms2-2 \
    liblcms2-dev \
    imagemagick-6-common \
    libgpm2 \
    libgpm-dev \
    libgconf2-4 \
    libgconf2-dev \
    libselinux1 \
    libselinux1-dev \
    libm17n-0 \
    libm17n-dev \
    libotf-dev \
    libsystemd0 \
    libsystemd-dev \
    libjansson4 \
    libjansson-dev \
    libgtk3.0-cil \
    libgtk3.0-cil-dev

RUN git clone https://github.com/emacs-mirror/emacs \
    && cd emacs \
    && ./autogen.sh \
    && ./configure \
    && make -j8 \
    && make install

CMD ["bash", "-c", "/bin/bash"]
