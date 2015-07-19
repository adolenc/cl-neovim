;;;; package.lisp

(defpackage #:cl-neovim
  (:nicknames #:nvim)
  (:use #:cl
        #:usocket
        #:messagepack
        #:babel)
  (:export send-command
           connect))

