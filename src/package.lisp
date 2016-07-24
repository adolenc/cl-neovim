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
  ;;; api.lisp
  (:export #:api-info               #:api-info/a
           #:buffer-add-highlight   #:buffer-add-highlight/a
           #:buffer-clear-highlight #:buffer-clear-highlight/a
           #:buffer-del-line        #:buffer-del-line/a
           #:buffer-insert          #:buffer-insert/a
           #:buffer-valid-p         #:buffer-valid-p/a
           #:buffer-line            #:buffer-line/a
           #:buffer-line-count      #:buffer-line-count/a
           #:buffer-lines           #:buffer-lines/a
           #:buffer-line-slice      #:buffer-line-slice/a
           #:buffer-mark            #:buffer-mark/a
           #:buffer-name            #:buffer-name/a
           #:buffer-number          #:buffer-number/a
           #:buffer-option          #:buffer-option/a
           #:buffers                #:buffers/a
           #:buffer-var             #:buffer-var/a
           #:call-function          #:call-function/a
           #:change-directory       #:change-directory/a
           #:color-map              #:color-map/a
           #:command                #:command/a
           #:command-output         #:command-output/a
           #:current-buffer         #:current-buffer/a
           #:current-line           #:current-line/a
           #:current-tabpage        #:current-tabpage/a
           #:current-window         #:current-window/a
           #:del-current-line       #:del-current-line/a
           #:err-write              #:err-write/a
           #:eval                   #:eval/a
           #:feedkeys               #:feedkeys/a
           #:input                  #:input/a
           #:list-runtime-paths     #:list-runtime-paths/a
           #:name-to-color          #:name-to-color/a
           #:option                 #:option/a
           #:out-write              #:out-write/a
           #:replace-termcodes      #:replace-termcodes/a
           #:report-error           #:report-error/a
           #:strwidth               #:strwidth/a
           #:subscribe              #:subscribe/a
           #:tabpage-valid-p        #:tabpage-valid-p/a
           #:tabpages               #:tabpages/a
           #:tabpage-var            #:tabpage-var/a
           #:tabpage-windows        #:tabpage-windows/a
           #:tabpage-window         #:tabpage-window/a
           #:unsubscribe            #:unsubscribe/a
           #:var                    #:var/a
           #:vvar                   #:vvar/a
           #:window-buffer          #:window-buffer/a
           #:window-cursor          #:window-cursor/a
           #:window-height          #:window-height/a
           #:window-valid-p         #:window-valid-p/a
           #:window-option          #:window-option/a
           #:window-position        #:window-position/a
           #:windows                #:windows/a
           #:window-tabpage         #:window-tabpage/a
           #:window-var             #:window-var/a
           #:window-width           #:window-width/a))
