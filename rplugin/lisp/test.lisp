(ql:quickload :cl-neovim-tests)
(in-package :cl-neovim-tests)
(in-suite neovim-test-suite)


; Show the progress of testing in neovim's statusline
(defmethod fiveam::%run :before ((suite fiveam::test-suite))
  (nvim:command (format nil "redraw | echo 'Lisp Host: running suite ~A'" (fiveam::name suite))))

(defmethod fiveam::%run :before ((test fiveam::test-case))
  (nvim:command (format nil "redraw | echo 'Lisp Host: running ~A'" (fiveam::name test))))


(nvim:defcommand/s lisp-host-run-tests (&rest args)
  (declare (opts (nargs "*")))
  (let ((test-results (with-output-to-string (fiveam:*test-dribble*)
                        (fiveam:run! 'neovim-test-suite))))
    (nvim:command (format nil "echo '~A'" test-results))
    args))
