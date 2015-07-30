;;;; package.lisp

(defpackage #:cl-neovim
  (:nicknames #:nvim)
  (:use #:cl
        #:babel
        #:messagepack
        #:split-sequence
        #:cl-async)
  (:export #:connect
           #:send-command))

