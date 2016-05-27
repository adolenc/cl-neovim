(ql:quickload :cl-neovim-tests :silent T)
(in-package :cl-neovim-tests)
(in-suite neovim-test-suite)


(nvim:defun/s lisp-host-test-1 ()
  "test1")

(test test-2
  (is (equal (format nil "~Ctest1" #\Newline)
             (nvim:command-output "echo LispHostTest1()"))))


(nvim:defcommand/s lisp-host-run-tests ()
   (let ((*standard-output* nvim:*debug-stream*)
         (*error-output* nvim:*debug-stream*))
     (fiveam:run! 'neovim-test-suite) (force-output)
     ; TODO: still results in rpc error (by printing to stdout?)
     T))
