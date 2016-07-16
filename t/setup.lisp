(in-package :cl-neovim-tests)
(def-suite callback-test-suite :description "Test suite for cl-neovim callbacks")
(def-suite api-test-suite :description "Test suite for cl-neovim api")


(unless nvim::*using-host*
  (nvim:connect :file "/tmp/nvim"))

(defun get-result-from-nvim (&optional (var-name "lisp_host_test_tmp_result"))
  (nvim:call/s t "vim_get_var" var-name))

(defun set-result-in-nvim (result &optional (var-name "lisp_host_test_tmp_result"))
  (nvim:call/s t "vim_set_var" var-name result))

(defmacro result-from-nvim/s (&body body)
  `(progn
     ,@body
     (get-result-from-nvim)))

(defmacro result-from-nvim/a (&body body)
  `(progn
     (set-result-in-nvim "result not set")
     ,@body
     (loop for result = (get-result-from-nvim) then (get-result-from-nvim)
           while (and (stringp result) (string= result "result not set"))
           do (sleep 0.05)
           finally (return result))))
