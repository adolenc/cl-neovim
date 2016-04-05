(in-package :cl-neovim-tests)


(def-suite neovim-test-suite :description "Test suite for cl-neovim")
(in-suite neovim-test-suite)


(nvim:defun lisp-host-test-1 :sync ()
  "test1")

(defun hmmm ()
  (format t "hmmm ~A~%" nvim::*using-host*))


(test sample-test
  (is (equal "test1" (nvim:command "call LispHostTest1()")))
  (is (equal 6 (nvim:strwidth "ngあの"))))
