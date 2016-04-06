(in-package :cl-user)

(defpackage #:msgpack-rpc
  (:nicknames #:mrpc)
  (:use #:cl
        #:messagepack
        #:bordeaux-threads 
        #:cl-async)
  (:export #:*extended-types*
           #:define-extension-types
           #:register-request-callback
           #:register-notification-callback
           #:remove-request-callback
           #:remove-notification-callback
           #:connect
           #:request
           #:notify
           #:finish))

(defpackage #:cl-neovim
  (:nicknames #:nvim)
  (:use #:cl
        #:msgpack-rpc
        #:form-fiddle
        #:split-sequence)
  (:shadow #:defun
           #:connect)
  (:export #:connect
           #:defcommand
           #:defautocmd
           #:defun
           #:*debug-stream*
           #:finish))
