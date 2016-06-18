(in-package #:cl-neovim)


(defparameter *log-stream* *standard-output*)
(defvar *using-host* NIL "Variable that host binds to T when it loads plugins.")

(defvar *specs* NIL "A list of all the specs nvim needs.")
(defvar *path* "" "Variable that gets set to path to plugin.")
(defvar *nvim-types* (mrpc:define-extension-types
                       '(0
                         Buffer
                         Window
                         Tabpage)))
(defvar *nvim-instance* NIL "Binds to the last connection to neovim")

(defclass nvim (mrpc:client)
  ((client-id :initform NIL :accessor client-id)))


(cl:defun connect (&rest args &key host port file)
  (let ((mrpc:*extended-types* *nvim-types*))
    (setf *nvim-instance* (apply #'make-instance 'nvim args))))

(cl:defun listen-once (&optional (instance *nvim-instance*))
  "Block execution listening for a new message for instance."
  (mrpc::run-once (mrpc::event-loop instance)))

(cl:defun %call (instance fn-type command &rest args)
  (let ((mrpc:*extended-types* *nvim-types*)
        (instance (etypecase instance
                    ((member t) *nvim-instance*)
                    (nvim instance))))
    (apply fn-type instance command args)))

(cl:defun call/s (instance command &rest args)
  "Send nvim command to neovim socket and return the result."
  (apply #'%call instance #'mrpc:request command args))

(cl:defun call/a (instance command &rest args)
  "Send nvim command to neovim socket asynchronously, returning the control
back to the caller immediately and discarding all return values/errors."
  (apply #'%call instance #'mrpc:notify command args))
