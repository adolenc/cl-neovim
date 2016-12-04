(ql:quickload :cl-neovim-tests)
(in-package :cl-neovim-tests)


; Show the progress of testing in neovim's statusline
(defmethod fiveam::%run :before ((suite fiveam::test-suite))
  (nvim:command (format nil "redraw | echo 'Lisp Host: running suite ~A'" (fiveam::name suite))))

(defmethod fiveam::%run :before ((test fiveam::test-case))
  (nvim:command (format nil "redraw | echo 'Lisp Host: running ~A'" (fiveam::name test))))


(nvim:defcommand lisp-host-run-tests (&optional filename)
  (declare (opts (nargs "?") (complete "file")))
  (let* ((test-results (list (fiveam:run 'api-low-level-test-suite)
                             (fiveam:run 'callback-test-suite)
                             (fiveam:run 'api-vim-test-suite)
                             (fiveam:run 'api-buffer-test-suite)
                             (fiveam:run 'api-window-test-suite)
                             (fiveam:run 'api-tabpage-test-suite)
                             (fiveam:run 'api-other-test-suite)))
         (test-details (with-output-to-string (fiveam:*test-dribble*)
                         (mapcar #'fiveam:explain! test-results)))
         (success (every #'fiveam:results-status test-results)))
    (format t "~&~A~%~%Success: ~A~%" test-details success)
    (if filename
      (progn (if (not success)
               (with-open-file (file filename :direction :output :if-does-not-exist :create :if-exists :overwrite)
                 (format file "~A" test-details)))
             (nvim:command/a "qa!"))
      (nvim:command (format nil "redraw | echo 'Lisp Host: done. Results:~%~A'" test-details)))))
