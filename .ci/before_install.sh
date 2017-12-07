#!/usr/bin/env bash
set -euo pipefail


# libuv1-dev
if [ ! -e $HOME/libuv-v1.9.1/Makefile ]; then
  cd $HOME
  wget archive.ubuntu.com/ubuntu/pool/universe/libu/libuv1/libuv1_1.9.1.orig.tar.gz
  tar -xzf libuv1_1.9.1.orig.tar.gz
  cd libuv-v1.9.1
  sh autogen.sh
  ./configure
  make
  # make check
fi
cd $HOME/libuv-v1.9.1
sudo make install

# roswell
curl -L https://raw.githubusercontent.com/snmsts/roswell/release/scripts/install-for-ci.sh | sh

# support for coveralls
git clone https://github.com/fukamachi/cl-coveralls $HOME/lisp/cl-coveralls

# up-to-date cl-messagepack and cl-messagepack-rpc
git clone https://github.com/mbrezu/cl-messagepack $HOME/lisp/cl-messagepack
git clone https://github.com/adolenc/cl-messagepack-rpc $HOME/lisp/cl-messagepack-rpc

# neovim
eval "$(curl -Ss https://raw.githubusercontent.com/neovim/bot-ci/master/scripts/travis-setup.sh) nightly-x64"
curl -fLo $HOME/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
cd "${TRAVIS_BUILD_DIR}"
