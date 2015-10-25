(load "~/.sbclrc")
(ql:quickload :cl-neovim :silent t)


(defparameter *err* "/tmp/err.log")

(defun load-plugin (path)
  (with-output-to-string (*standard-output*)
    (setf nvim::*path* path)
    (load path)))

(nvim:defun "specs" :sync (path)
  (with-open-file (*error-output* *err* :direction :output :if-does-not-exist :create :if-exists :append)
    (setf nvim::*specs* NIL)
    (load-plugin path))
  (make-array (length nvim::*specs*) :initial-contents nvim::*specs*))

(nvim:defun "poll" :sync ()
  "ok")


(if (rest *posix-argv*)
  (mapcar #'load-plugin (rest *posix-argv*)))

(nvim:connect)
