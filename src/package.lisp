(in-package :cl-user)

(defpackage #:cl-neovim
  (:nicknames #:nvim)
  (:use #:cl
        #:messagepack-rpc
        #:form-fiddle
        #:split-sequence)
  (:shadow #:defun
           #:funcall
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
           #:funcall        #:funcall/a
           #:call-atomic)
  ;;; generated-api.lisp
  (:export #:buffer-line-count #:buffer-lines #:buffer-var #:buffer-del-var #:buffer-option #:buffer-name #:buffer-valid-p #:buffer-mark #:buffer-add-highlight #:buffer-clear-highlight #:tabpage-windows #:tabpage-var #:tabpage-del-var #:tabpage-window #:tabpage-number #:tabpage-valid-p #:ui-attach #:ui-detach #:ui-try-resize #:ui-option #:command #:feedkeys #:input #:replace-termcodes #:command-output #:eval #:call-function #:strwidth #:runtime-paths #:current-dir #:current-line #:del-current-line #:var #:del-var #:vvar #:option #:out-write #:err-write #:err-writeln #:buffers #:current-buffer #:windows #:current-window #:tabpages #:current-tabpage #:color-by-name #:color-map #:api-info #:window-buffer #:window-cursor #:window-height #:window-width #:window-var #:window-del-var #:window-option #:window-position #:window-tabpage #:window-number #:window-valid-p #:buffer-line #:buffer-del-line #:buffer-line-slice #:buffer-insert #:change-directory #:report-error #:name-to-color)
  (:export #:buffer-line-count/a #:buffer-lines/a #:buffer-var/a #:buffer-del-var/a #:buffer-option/a #:buffer-name/a #:buffer-valid-p/a #:buffer-mark/a #:buffer-add-highlight/a #:buffer-clear-highlight/a #:tabpage-windows/a #:tabpage-var/a #:tabpage-del-var/a #:tabpage-window/a #:tabpage-number/a #:tabpage-valid-p/a #:ui-attach/a #:ui-detach/a #:ui-try-resize/a #:ui-option/a #:command/a #:feedkeys/a #:input/a #:replace-termcodes/a #:command-output/a #:eval/a #:call-function/a #:strwidth/a #:runtime-paths/a #:current-dir/a #:current-line/a #:del-current-line/a #:var/a #:del-var/a #:vvar/a #:option/a #:out-write/a #:err-write/a #:err-writeln/a #:buffers/a #:current-buffer/a #:windows/a #:current-window/a #:tabpages/a #:current-tabpage/a #:color-by-name/a #:color-map/a #:api-info/a #:window-buffer/a #:window-cursor/a #:window-height/a #:window-width/a #:window-var/a #:window-del-var/a #:window-option/a #:window-position/a #:window-tabpage/a #:window-number/a #:window-valid-p/a #:buffer-line/a #:buffer-del-line/a #:buffer-line-slice/a #:buffer-insert/a #:change-directory/a #:report-error/a #:name-to-color/a)
  )
