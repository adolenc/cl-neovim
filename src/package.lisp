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
           #:defcommand/a
           #:defautocmd
           #:defautocmd/s
           #:defautocmd/a
           #:defun
           #:defun/s
           #:defun/a
           #:call/s
           #:call/a
           #:*debug-stream*))
