(in-package :cl-user)

(defpackage #:cl-neovim
  (:nicknames #:nvim)
  (:use #:cl
        #:messagepack-rpc
        #:form-fiddle
        #:split-sequence)
  (:shadow #:defun
           #:connect)
  (:export #:connect
           #:defcommand
           #:defautocmd
           #:defun
           #:*debug-stream*))
