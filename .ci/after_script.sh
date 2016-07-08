#!/usr/bin/env bash
set -euo pipefail


if [[ "${TEST_TARGET}" == repl ]]; then
  kill -SIGINT $NVIM_PID
fi
