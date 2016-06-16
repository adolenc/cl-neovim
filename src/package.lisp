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
  (:export #:*debug-stream*
           #:connect
           #:listen-once
           #:call/s
           #:call/a)
  ;;; callbacks.lisp
  (:export #:defcommand #:defcommand/s
           #:defautocmd #:defautocmd/s
           #:defun      #:defun/s)
  ;;; api.lisp
  (:export #:api-info               #:api-info/s               #:api-info/a
           #:buffer-add-highlight   #:buffer-add-highlight/s   #:buffer-add-highlight/a
           #:buffer-clear-highlight #:buffer-clear-highlight/s #:buffer-clear-highlight/a
           #:buffer-del-line        #:buffer-del-line/s        #:buffer-del-line/a
           #:buffer-insert          #:buffer-insert/s          #:buffer-insert/a
           #:buffer-is-valid        #:buffer-is-valid/s        #:buffer-is-valid/a
           #:buffer-line            #:buffer-line/s            #:buffer-line/a
           #:buffer-line-count      #:buffer-line-count/s      #:buffer-line-count/a
           #:buffer-lines           #:buffer-lines/s           #:buffer-lines/a
           #:buffer-line-slice      #:buffer-line-slice/s      #:buffer-line-slice/a
           #:buffer-mark            #:buffer-mark/s            #:buffer-mark/a
           #:buffer-name            #:buffer-name/s            #:buffer-name/a
           #:buffer-number          #:buffer-number/s          #:buffer-number/a
           #:buffer-option          #:buffer-option/s          #:buffer-option/a
           #:buffers                #:buffers/s                #:buffers/a
           #:buffer-var             #:buffer-var/s             #:buffer-var/a
           #:call-function          #:call-function/s          #:call-function/a
           #:change-directory       #:change-directory/s       #:change-directory/a
           #:color-map              #:color-map/s              #:color-map/a
           #:command                #:command/s                #:command/a
           #:command-output         #:command-output/s         #:command-output/a
           #:current-buffer         #:current-buffer/s         #:current-buffer/a
           #:current-line           #:current-line/s           #:current-line/a
           #:current-tabpage        #:current-tabpage/s        #:current-tabpage/a
           #:current-window         #:current-window/s         #:current-window/a
           #:del-current-line       #:del-current-line/s       #:del-current-line/a
           #:err-write              #:err-write/s              #:err-write/a
           #:eval                   #:eval/s                   #:eval/a
           #:feedkeys               #:feedkeys/s               #:feedkeys/a
           #:input                  #:input/s                  #:input/a
           #:list-runtime-paths     #:list-runtime-paths/s     #:list-runtime-paths/a
           #:name-to-color          #:name-to-color/s          #:name-to-color/a
           #:option                 #:option/s                 #:option/a
           #:out-write              #:out-write/s              #:out-write/a
           #:replace-termcodes      #:replace-termcodes/s      #:replace-termcodes/a
           #:report-error           #:report-error/s           #:report-error/a
           #:strwidth               #:strwidth/s               #:strwidth/a
           #:subscribe              #:subscribe/s              #:subscribe/a
           #:tabpage-is-valid       #:tabpage-is-valid/s       #:tabpage-is-valid/a
           #:tabpages               #:tabpages/s               #:tabpages/a
           #:tabpage-var            #:tabpage-var/s            #:tabpage-var/a
           #:tabpage-windows        #:tabpage-windows/s        #:tabpage-windows/a
           #:tabpage-window         #:tabpage-window/s         #:tabpage-window/a
           #:unsubscribe            #:unsubscribe/s            #:unsubscribe/a
           #:var                    #:var/s                    #:var/a
           #:vvar                   #:vvar/s                   #:vvar/a
           #:window-buffer          #:window-buffer/s          #:window-buffer/a
           #:window-cursor          #:window-cursor/s          #:window-cursor/a
           #:window-height          #:window-height/s          #:window-height/a
           #:window-is-valid        #:window-is-valid/s        #:window-is-valid/a
           #:window-option          #:window-option/s          #:window-option/a
           #:window-position        #:window-position/s        #:window-position/a
           #:windows                #:windows/s                #:windows/a
           #:window-tabpage         #:window-tabpage/s         #:window-tabpage/a
           #:window-var             #:window-var/s             #:window-var/a
           #:window-width           #:window-width/s           #:window-width/a))
