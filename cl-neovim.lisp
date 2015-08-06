(in-package #:cl-neovim)

(defparameter *nvim-types* (mrpc:define-extension-types
                             '(0
                               Buffer
                               Window
                               Tabpage)))

(defmacro defcallback (name args &body body)
  `(mrpc:register-request-callback ,(symbol-name name) #'(lambda ,args ,@body)))

(defun send-command (command &rest args)
  "Send nvim command to neovim socket and return the result."
  (let ((mrpc:*extended-types* *nvim-types*))
    (mrpc:request command args)))

(defun connect (&rest args &key host port filename)
  (let ((mrpc:*extended-types* *nvim-types*))
    (apply #'mrpc:run args)))
