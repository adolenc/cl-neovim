(ql:quickload :cl-neovim-tests :silent T)
(in-package :cl-neovim-tests)
(in-suite neovim-test-suite)


(nvim:defun/s lisp-host-test-1 ()
  "test1")

(test test-2
  (is (equal (format nil "~Ctest1" #\Newline)
             (nvim:command-output "echo LispHostTest1()"))))


(nvim:defcommand/s lisp-host-run-tests (&rest args)
  (declare (opts (nargs "*")))
  (let ((test-results (with-output-to-string (fiveam:*test-dribble*)
                        (fiveam:run! 'neovim-test-suite))))
    (nvim:command (format nil "echo '~A'" test-results))
    args))
