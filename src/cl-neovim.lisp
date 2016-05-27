(in-package #:cl-neovim)


(defparameter *debug-stream* (make-broadcast-stream))
(defvar *using-host* NIL "Variable that host binds to T when it loads plugins.")

(defvar *specs* NIL "A list of all the specs nvim needs.")
(defvar *path* NIL "A list of all the specs nvim needs.")
(defvar *nvim-instance* NIL "Instance of connection to neovim")
(defvar *nvim-types* (mrpc:define-extension-types
                       '(0
                         Buffer
                         Window
                         Tabpage)))

(cl:defun send-command (command async &rest args)
  "Send nvim command to neovim socket and return the result."
  (let ((mrpc:*extended-types* *nvim-types*))
    (if async
      (apply #'mrpc:notify *nvim-instance* command args)
      (apply #'mrpc:request *nvim-instance* command args))))

(cl:defun connect (&rest args &key host port file)
  (let ((mrpc:*extended-types* *nvim-types*))
    (setf *nvim-instance* (apply #'make-instance 'mrpc:client args))))
