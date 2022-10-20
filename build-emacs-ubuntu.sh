#!/bin/sh

sudo apt install ca-certificates ripgrep fd-find libvterm-bin libvterm-dev cmake libtool libssl-dev libxpm-dev libgif-dev libjpeg-dev libpng-dev libtiff-dev libx11-dev libncurses5-dev automake autoconf texinfo libgtk2.0-dev gnutls-bin gnutls-dev libgtk-3-dev libxpm-dev libgnutls28-dev libwebkit2gtk-4.0-dev libwebkit2gtk-4.0-37 librsvg2-2 librsvg2-bin librsvg2-common librsvg2-dev libmagickcore-6.q16-6 libmagickcore-6.q16-dev libmagickwand-6.q16-6 libmagickwand-6.q16-dev libmagickwand-6-headers
sudo add-apt-repository --yes ppa:ubuntu-toolchain-r/ppa
sudo apt install gcc-12 g++-12 libgccjit0 libgccjit-12-dev libjansson4 libjansson-dev
git clone --depth 1 https://github.com/emacs-mirror/emacs emacs || true
#git clone --depth 1 https://github.com/flatwhatson/emacs -b pgtk-nativecomp emacs || true
cd emacs
sudo make uninstall || true
sudo make extraclean || true
git reset --hard
git clean -xdf
git pull
export CC=/usr/bin/gcc-12
export CXX=/usr/bin/gcc-12
export CFLAGS=" -O3 -pipe -mtune=native -march=native -fomit-frame-pointer"
export CXXFLAGS=" -O3 -pipe -mtune=native -march=native -fomit-frame-pointer"
./autogen.sh
./configure \
        --with-gnutls \
        --with-harfbuzz \
        --with-json \
        --with-xml2 \
        --with-modules \
        --with-native-compilation=aot \
        --with-imagemagick \
        --with-zlib \
        --with-xwidgets
make -j20
sudo make install
