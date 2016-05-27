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
           #:defcommand/s
           #:defautocmd
           #:defautocmd/s
           #:defun
           #:defun/s
           #:*debug-stream*))
