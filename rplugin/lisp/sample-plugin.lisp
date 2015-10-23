(defpackage #:sample-plugin
  (:use #:cl #:cl-neovim))
(in-package #:sample-plugin)

(defparameter *calls* 0 "Counter for calls.")

(nvim:defcmd cmd :sync (&rest args &opts (range r))
  (declare (opts (range "%") (nargs "*") (complete "file")))
  (increment-calls)
  (setf (nvim:current-line) (format nil "Command: Called ~A times, args: ~A, range: ~A" *calls* args r)))

(nvim:defautocmd buf-enter :sync (filename)
  (declare (opts (pattern "*.lisp") (eval "expand(\"<afile>\")")))
  (increment-calls)
  (setf (nvim:current-line) (format nil "Autocmd: Called ~A times, file: ~A" *calls* filename))) 

(nvim:defunc func (&rest args)
  (increment-calls)
  (setf (nvim:current-line) (format nil "Function: Called ~A times, args: ~A" *calls* args)))

(defun increment-calls ()
  (if (= *calls* 5)
    (error "Too many calls!")
    (incf *calls*)))
