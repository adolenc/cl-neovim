;;;; package.lisp

(defpackage #:cl-neovim
  (:nicknames #:nvim)
  (:use #:cl
        #:babel
        #:messagepack
        #:usocket
        #:anaphora)
  (:export #:connect
           #:send-command))

