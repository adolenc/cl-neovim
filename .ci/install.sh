#!/usr/bin/env bash
set -euo pipefail


mkdir -p $HOME/.config/nvim/plugged/
ln -s "${TRAVIS_BUILD_DIR}" $HOME/.config/nvim/plugged/cl-neovim
cp "${TRAVIS_BUILD_DIR}/.ci/minimal_init.vim" $HOME/.config/nvim/init.vim
