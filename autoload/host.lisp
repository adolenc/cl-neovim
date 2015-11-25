#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :cl-neovim :silent t)


(defparameter *dbg-stream* NIL)

(defun load-plugin (path)
  (setf nvim::*path* path)
  (load path))

(nvim:defun "specs" :sync (path)
  (declare (opts ignore))
  (let ((nvim::*specs* '()))
    (load-plugin path)
    nvim::*specs*))

(nvim:defun poll :sync ()
  (declare (opts ignore))
  "ok")

(nvim:defun enable-debugging :sync (filename)
  (declare (opts ignore))
  (setf *dbg-stream* filename))

(nvim:defun load-plugins :sync (plugins)
  (declare (opts ignore))
  (with-open-file (*standard-output* (or *dbg-stream* "/dev/null") :direction :output :if-does-not-exist :create :if-exists :append)
    (let ((*error-output* *standard-output*))
      (map NIL #'load-plugin plugins))))

(nvim:connect)
