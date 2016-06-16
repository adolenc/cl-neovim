(in-package :cl-user)

(defpackage #:cl-neovim
  (:nicknames #:nvim)
  (:use #:cl
        #:messagepack-rpc
        #:form-fiddle
        #:jonathan
        #:split-sequence)
  (:shadow #:defun
           #:eval
           #:connect)
  (:export #:connect
           #:listen-once
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
