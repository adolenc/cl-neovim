;;;; package.lisp

(defpackage #:cl-neovim
  (:nicknames #:nvim)
  (:use #:cl
        #:babel
        #:messagepack
        #:usocket
        #:split-sequence
        #:anaphora)
  (:export #:connect
           #:send-command))

