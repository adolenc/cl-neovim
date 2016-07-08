#!/usr/bin/env bash
set -euo pipefail


export PATH="${TRAVIS_BUILD_DIR}/_neovim/bin:${PATH}" # Not sure why this is not preserved between travis steps, but whatever
export VIM="${TRAVIS_BUILD_DIR}/_neovim/share/nvim/runtime"

if [[ "${TEST_TARGET}" == repl ]]; then
  nvim --headless -i NONE -N -n & export NVIM_PID=$!
else
  nvim --headless -i NONE -N -n -c "UpdateRemotePlugins" -c "q"
  cat "${HOME}/.config/nvim/.init.vim-rplugin~"
fi
