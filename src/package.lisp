(in-package :cl-user)

(defpackage #:cl-neovim
  (:nicknames #:nvim)
  (:use #:cl
        #:messagepack-rpc
        #:form-fiddle
        #:split-sequence)
  (:shadow #:defun
           #:eval
           #:connect)

  ;;; cl-neovim.lisp
  (:export #:*nvim-instance*
           #:connect
           #:listen-once
           #:call/s
           #:call/a)
  ;;; logging.lisp
  (:export #:enable-logging)
  ;;; callbacks.lisp
  (:export #:defcommand #:defcommand/s
           #:defautocmd #:defautocmd/s
           #:defun      #:defun/s)
  ;;; interface.lisp
  (:export #:buffer-number  #:buffer-number/a
           #:subscribe      #:subscribe/a
           #:unsubscribe    #:unsubscribe/a
           #:call-atomic)
  ;;; see also generated-api.lisp
  )
