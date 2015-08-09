;;;; package.lisp

(defpackage #:cl-msgpack-rpc
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
           #:run 
           #:request
           #:notify
           #:finish))

(defpackage #:cl-neovim
  (:nicknames #:nvim)
  (:use #:cl
        #:mrpc
        #:babel
        #:split-sequence)
  (:export #:connect
           #:defcallback
           #:defcmd
           #:defautocmd
           #:defn))
