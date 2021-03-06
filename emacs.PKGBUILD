pkgname="emacs-git"
pkgver=28.0.50.144748
pkgrel=1
pkgdesc="GNU Emacs. Development master branch."
arch=('x86_64' )
url="http://www.gnu.org/software/emacs/"
license=('GPL3' )
depends=('alsa-lib' 'gnutls' 'libxml2' 'jansson' 'm17n-lib' 'libotf' 'harfbuzz' 'gpm' 'gtk3' 'libjpeg-turbo' 'giflib' 'cairo' 'webkit2gtk' 'libgccjit' 'gcc' )
makedepends=('git' 'xorgproto')
provides=('emacs' 'emacs-seq')
conflicts=('emacs' 'emacs26-git' 'emacs-27-git' 'emacs-seq')
replaces=('emacs26-git' 'emacs27-git' 'emacs-seq')
#source=("emacs-git::git://git.savannah.gnu.org/emacs.git")
#source=("emacs-git::git://github.com/emacs-mirror/emacs.git#branch=feature/nativecomp")
source=("emacs-git::git://github.com/flatwhatson/emacs.git#branch=pgtk-nativecomp")
options=(!strip)
md5sums=('SKIP')

export CC='gcc'
export CXX='g++'
export LD="ld.gold"
export CFLAGS=" -fuse-ld=gold -flto -fuse-linker-plugin -O3 -mtune=native -march=native -fomit-frame-pointer"
export CXXFLAGS=" -fuse-ld=gold -flto -fuse-linker-plugin -O3 -mtune=native -march=native -fomit-frame-pointer"

################################################################################
pkgver() {
  cd "$srcdir/emacs-git"

  printf "%s.%s" \
    "$(grep AC_INIT configure.ac | \
    sed -e 's/^.\+\ \([0-9]\+\.[0-9]\+\.[0-9]\+\?\).\+$/\1/')" \
    "$(git rev-list --count HEAD)"
}

prepare() {
  cd "$srcdir/emacs-git"
  [[ -x configure ]] || ( ./autogen.sh git && ./autogen.sh autoconf )
}

build() {
  cd "$srcdir/emacs-git"

  local _conf=(
    --prefix=/usr
    --sysconfdir=/etc
    --libexecdir=/usr/lib
    --localstatedir=/var
    --mandir=/usr/share/man
    --with-cairo
    --with-gameuser=:games
    --with-gnutls
    --with-harfbuzz
    --with-json
    --with-modules
    --with-nativecomp
    --with-pgtk
    --with-rsvg
    --with-threads
    --with-x
    --with-x-toolkit=gtk3
    --with-xwidgets
    --with-zlib
    --without-compress-install
    --without-dbus
    --without-dconf
    --without-gconf
    --without-gsettings
    --without-makeinfo
    --without-mailutils
    --without-pop
    --without-sound
    --without-toolkit-scroll-bars
    --without-xaw3d
    --enable-link-time-optimization
  )

  ./configure "${_conf[@]}"
  make NATIVE_FULL_AOT=1 -j $(nproc)
}

package() {
  cd "$srcdir/emacs-git"

  make DESTDIR="$pkgdir/" install

  # remove conflict with ctags package
  mv "$pkgdir"/usr/bin/{ctags,ctags.emacs}

  mv "$pkgdir"/usr/share/man/man1/{ctags.1,ctags.emacs.1};
  find "$pkgdir"/usr/share/emacs/ | xargs chown root:root

  mkdir -p "$pkgdir"/var/games/emacs
  chmod 775 "$pkgdir"/var/games
  chmod 775 "$pkgdir"/var/games/emacs
  chown -R root:games "$pkgdir"/var/games

}

################################################################################
# vim:set ft=sh ts=2 sw=2 et:
