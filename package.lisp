;;;; package.lisp

(defpackage #:cl-msgpack-rpc
  (:nicknames #:mrpc)
  (:use #:cl
        #:messagepack
        #:bordeaux-threads 
        #:cl-async)
  (:export #:*intern-foreign-name-fn*
           #:*extended-types*
           #:define-extension-types
           #:register-callback
           #:remove-callback
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
           #:defcallback))
