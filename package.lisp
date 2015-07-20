;;;; package.lisp

(defpackage #:cl-neovim
  (:nicknames #:nvim)
  (:use #:cl
        #:babel
        #:messagepack
        #:usocket)
  (:export #:connect
           #:send-command))

