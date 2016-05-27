(defpackage #:sample-plugin
  (:use #:cl #:cl-neovim)
  (:shadowing-import-from #:cl #:defun))
(in-package #:sample-plugin)


(defparameter *calls* 0 "Counter for calls.")

(defun increment-calls ()
  (if (= *calls* 5)
    (error "Too many calls!")
    (incf *calls*)))

(nvim:defcommand/s lisp-sample-cmd (&rest args &opts (range r) bang)
  (declare (opts (range "%") (nargs "*") bang (complete "file")))
  (increment-calls)
  (setf (nvim:current-line) (format nil "Command: Called ~A times, args: ~A, range: ~A, bang: ~A" *calls* args r bang)))

(nvim:defautocmd/s buf-enter (filename)
  (declare (opts (pattern "*.lisp") (vim-eval "expand(\"<afile>\")")))
  (increment-calls)
  (setf (nvim:current-line) (format nil "Autocmd: Called ~A times, file: ~A" *calls* filename))) 

(nvim:defun "LispSampleFun" (&rest args &opts (vim-eval curr-line))
  (declare (opts (vim-eval "line(\".\")-1")))
  (increment-calls)
  (setf (nvim:current-line) (format nil "Function: Called ~A times, args: ~A, eval: ~A" *calls* args curr-line)))
