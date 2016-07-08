#!/usr/bin/env bash
set -euo pipefail


export PATH="${TRAVIS_BUILD_DIR}/_neovim/bin:${PATH}"
export VIM="${TRAVIS_BUILD_DIR}/_neovim/share/nvim/runtime"

if [[ "${TEST_TARGET}" == host ]]; then
  nvim --headless -i NONE -N -n -c "LispHostRunTests /tmp/results"
  cat $NVIM_LISP_LOG_FILE
  if [[ -e "/tmp/results" ]]; then
    cat /tmp/results
    cat $NVIM_LOG_FILE
    exit 1
  fi
else
  cl -l fiveam -l cl-coveralls \
     -e '(setf fiveam:*debug-on-error* t
               fiveam:*debug-on-failure* t)' \
     -e '(setf *debugger-hook*
               (lambda (c h)
                 (declare (ignore c h))
                 (uiop:quit -1)))' \
     -e '(coveralls:with-coveralls (:exclude (list "t"))
           (ql:quickload :cl-neovim-tests)
           (asdf:test-system :cl-neovim))'
fi
