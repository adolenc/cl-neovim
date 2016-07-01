(in-package :cl-user)

(ql:quickload :arrow-macros)
(defpackage #:lisp-interface
  (:use #:cl #:cl-neovim #:arrow-macros)
  (:shadowing-import-from #:cl #:defun #:eval))
(in-package :lisp-interface)

(nvim:defcommand/s lisp (&rest form)
  (declare (opts nargs))
  (let ((output (with-output-to-string (*standard-output*)
                  (->> form  ; == '("(+" "1" "2)")
                    (format nil "~{~A~^ ~}")
                    read-from-string
                    eval))))
    (nvim:command (format nil "echo '~A'" output))))


(in-package :cl-user)
