(in-package :cl-neovim-tests)
(in-suite neovim-test-suite)


(nvim:defcommand/s "LispHostTestSameCallbackName" ()
  (declare (opts bang))
  (set-result-in-nvim "first cmd"))

(nvim:defcommand/s "LispHostTestSameCallbackName" ()
  (set-result-in-nvim "second cmd"))

(nvim:defun/s "LispHostTestSameCallbackName" ()
  (set-result-in-nvim "first fun"))

(test registering-duplicate-callbacks
  (is (equal "second cmd" (progn (nvim:command "LispHostTestSameCallbackName")
                                 (get-result-from-nvim))))
  (signals mrpc:rpc-error (nvim:command "LispHostTestSameCallbackName!"))
  (is (equal "first fun" (progn (nvim:call-function "LispHostTestSameCallbackName" #())
                                (get-result-from-nvim)))))

