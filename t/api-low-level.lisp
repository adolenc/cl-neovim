(in-package :cl-neovim-tests)
(in-suite api-low-level-test-suite)


(test manual-sync-calls
  (is (= (nvim:call/s t "vim_eval" "3 + 2") 5))
  (is (= (nvim:call/s nvim:*nvim-instance* "vim_eval" "3 + 2") 5))
  (signals error (nvim:call/s NIL "vim_eval" "3 + 2")))

(test manual-async-calls
  (is (= 3 (result-from-nvim/a (nvim:call/a t "vim_set_var" "lisp_host_test_tmp_result" 3)))))
