(in-package :cl-user)

(defpackage #:lisp-interface
  (:use #:cl #:cl-neovim)
  (:shadowing-import-from #:cl #:defun #:eval))
(in-package :lisp-interface)


(defmacro echo-output (&body forms)
  (let ((output (gensym)))
    `(let ((,output (with-output-to-string (*standard-output*)
                     ,@forms)))
       (nvim:command (format nil "echo '~A'" ,output)))))

(defun eval-string (str)
  (eval (read-from-string str)))


(nvim:defcommand/s lisp (&rest form)
  (declare (opts nargs))
  (echo-output (eval-string (format nil "~{~A~^ ~}" form))))


(in-package :cl-user)
