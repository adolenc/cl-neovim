(in-package #:cl-neovim)

(defparameter *nvim-type-list* (define-extension-types
                                 '(0
                                   Buffer
                                   Window
                                   Tabpage)))

(defmacro defcallback (name args &body body)
  `(register-callback ',name #'(lambda ,args ,@body)))

(defun send-command (command &rest args)
  "Send nvim command to neovim socket and return the result."
  (let ((*decoder-prefers-lists* T)
        (*extended-types* *nvim-type-list*))
    (request command args)))
