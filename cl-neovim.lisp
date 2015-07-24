(in-package #:cl-neovim)

(defparameter *nvim-type-list* (define-extension-types
                                 '(0
                                   Buffer
                                   Window
                                   Tabpage)))

(defun send-command (command &rest args)
  "Send nvim command to neovim socket and return the result."
  (unless *socket* (connect))
  (let ((*decoder-prefers-lists* T)
        (*extended-types* *nvim-type-list*))
    (send-msg (make-rpc-request command (or args #())))
    (multiple-value-bind (msg-id msg-result) (get-result)
      msg-result)))
