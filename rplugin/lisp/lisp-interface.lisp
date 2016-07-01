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

(nvim:defcommand/s lispdo (&rest form &opts range)
  (declare (opts nargs range))
  (let ((fn (eval-string
              (format nil "#'(lambda (line line-nr)
                               (declare (ignorable line line-nr))
                               ~{~A~^ ~})"
                      form)))
        (start (1- (first range)))
        (end (second range)))
    (echo-output
      (let ((new-lines (mapcar #'(lambda (line line-nr)
                                   (or (let ((new-line (funcall fn line line-nr)))
                                         (if (or (stringp new-line) (null new-line))
                                           new-line
                                           (error "Function must return either string or NIL.")))
                                       line))
                               (nvim:buffer-lines (nvim:current-buffer) start end T)
                               (loop for line-nr from start upto end collect (1+ line-nr)))))
        (setf (nvim:buffer-lines (nvim:current-buffer) start end T) new-lines)))))

(in-package :cl-user)
