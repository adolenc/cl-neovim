(in-package #:cl-neovim)


(eval-when (:compile-toplevel)

(defparameter *dangerous-names* '("vim_eval"))

(defun string->symbol (str) "Convert string into symbol." (intern (substitute #\- #\_ (format nil "~:@(~A~)" str)))) 

(defun parse-args (args)
  "Extract names from nvim api's metadata of arguments into a list of symbols."
  (cond ((listp args) (mapcar #'(lambda (arg) (string->symbol (second arg))) args))
        ((stringp args) (list (string->symbol args)))
        (t NIL)))

(defun setterp (name) "Is name a setter?" (search "set_" name))

(defun clean-up-name (name &optional (modifiers '("vim" "get" "set")))
  "Removes all substrings specified in modifiers from name."
  (let* ((components (split-sequence #\_ name))
         (main-components (remove-if #'(lambda (c) (member c modifiers :test #'string=)) components)))
    (format nil "~{~A~^_~}" main-components)))

(defun symbol-append (&rest symbols) 
  "Concatenate symbol names and return resulting symbol."
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))

) ; end of eval-when

(defmacro mdata->lisp-function (name args ret can-fail deferred)
  "Create and export functions from the parsed nvim's api."
  (declare (ignore ret can-fail deferred))
  (let* ((args (parse-args args))
         (n (string->symbol (if (member name *dangerous-names* :test #'string-equal) name (clean-up-name name))))
         (async-n (symbol-append n '-a))
         (sync-n (symbol-append n '-s)))
    (if (setterp name)
      `(progn (defun (setf ,n) (,@(last args) ,@(butlast args))
                (funcall #'send-command ,name T ,@args)) 
              (defun (setf ,sync-n) (,@(last args) ,@(butlast args))
                (funcall #'send-command ,name NIL ,@args))
              (export ',sync-n :cl-neovim)
              (export ',n :cl-neovim))
      `(progn (defun ,n ,args
                (funcall #'send-command ,name NIL ,@args))
              (defun ,async-n ,args
                (funcall #'send-command ,name T ,@args))
              (export ',async-n :cl-neovim)
              (export ',n :cl-neovim)))))

;;;; Rest of file generated with `generate-api.lisp'.
(mdata->lisp-function "window_get_buffer" (("Window" "window")) "Buffer" T NIL)
(mdata->lisp-function "window_get_cursor" (("Window" "window"))
                     "ArrayOf(Integer, 2)" T NIL)
(mdata->lisp-function "window_set_cursor"
                     (("Window" "window") ("ArrayOf(Integer, 2)" "pos")) "void" T
                     T)
(mdata->lisp-function "window_get_height" (("Window" "window")) "Integer" T NIL)
(mdata->lisp-function "window_set_height"
                     (("Window" "window") ("Integer" "height")) "void" T T)
(mdata->lisp-function "window_get_width" (("Window" "window")) "Integer" T NIL)
(mdata->lisp-function "window_set_width" (("Window" "window") ("Integer" "width"))
                     "void" T T)
(mdata->lisp-function "window_get_var" (("Window" "window") ("String" "name"))
                     "Object" T NIL)
(mdata->lisp-function "window_set_var"
                     (("Window" "window") ("String" "name") ("Object" "value"))
                     "Object" T T)
(mdata->lisp-function "window_get_option" (("Window" "window") ("String" "name"))
                     "Object" T NIL)
(mdata->lisp-function "window_set_option"
                     (("Window" "window") ("String" "name") ("Object" "value"))
                     "void" T T)
(mdata->lisp-function "window_get_position" (("Window" "window"))
                     "ArrayOf(Integer, 2)" T NIL)
(mdata->lisp-function "window_get_tabpage" (("Window" "window")) "Tabpage" T NIL)
(mdata->lisp-function "window_is_valid" (("Window" "window")) "Boolean" NIL NIL)
(mdata->lisp-function "vim_command" (("String" "str")) "void" T T)
(mdata->lisp-function "vim_feedkeys"
                     (("String" "keys") ("String" "mode")
                                        ("Boolean" "escape_csi"))
                     "void" NIL T)
(mdata->lisp-function "vim_input" (("String" "keys")) "Integer" NIL NIL)
(mdata->lisp-function "vim_replace_termcodes"
                     (("String" "str") ("Boolean" "from_part")
                                       ("Boolean" "do_lt") ("Boolean" "special"))
                     "String" NIL NIL)
(mdata->lisp-function "vim_command_output" (("String" "str")) "String" T NIL)
(mdata->lisp-function "vim_eval" (("String" "str")) "Object" T T)
(mdata->lisp-function "vim_strwidth" (("String" "str")) "Integer" T NIL)
(mdata->lisp-function "vim_list_runtime_paths" NIL "ArrayOf(String)" NIL NIL)
(mdata->lisp-function "vim_change_directory" (("String" "dir")) "void" T NIL)
(mdata->lisp-function "vim_get_current_line" NIL "String" T NIL)
(mdata->lisp-function "vim_set_current_line" (("String" "line")) "void" T T)
(mdata->lisp-function "vim_del_current_line" NIL "void" T T)
(mdata->lisp-function "vim_get_var" (("String" "name")) "Object" T NIL)
(mdata->lisp-function "vim_set_var" (("String" "name") ("Object" "value"))
                     "Object" T T)
(mdata->lisp-function "vim_get_vvar" (("String" "name")) "Object" T NIL)
(mdata->lisp-function "vim_get_option" (("String" "name")) "Object" T NIL)
(mdata->lisp-function "vim_set_option" (("String" "name") ("Object" "value"))
                     "void" T T)
(mdata->lisp-function "vim_out_write" (("String" "str")) "void" NIL T)
(mdata->lisp-function "vim_err_write" (("String" "str")) "void" NIL T)
(mdata->lisp-function "vim_report_error" (("String" "str")) "void" NIL T)
(mdata->lisp-function "vim_get_buffers" NIL "ArrayOf(Buffer)" NIL NIL)
(mdata->lisp-function "vim_get_current_buffer" NIL "Buffer" NIL NIL)
(mdata->lisp-function "vim_set_current_buffer" (("Buffer" "buffer")) "void" T T)
(mdata->lisp-function "vim_get_windows" NIL "ArrayOf(Window)" NIL NIL)
(mdata->lisp-function "vim_get_current_window" NIL "Window" NIL NIL)
(mdata->lisp-function "vim_set_current_window" (("Window" "window")) "void" T T)
(mdata->lisp-function "vim_get_tabpages" NIL "ArrayOf(Tabpage)" NIL NIL)
(mdata->lisp-function "vim_get_current_tabpage" NIL "Tabpage" NIL NIL)
(mdata->lisp-function "vim_set_current_tabpage" (("Tabpage" "tabpage")) "void" T
                     T)
(mdata->lisp-function "vim_subscribe" (("String" "event")) "void" NIL NIL)
(mdata->lisp-function "vim_unsubscribe" (("String" "event")) "void" NIL NIL)
(mdata->lisp-function "vim_name_to_color" (("String" "name")) "Integer" NIL NIL)
(mdata->lisp-function "vim_get_color_map" NIL "Dictionary" NIL NIL)
(mdata->lisp-function "vim_get_api_info" NIL "Array" NIL NIL)
(mdata->lisp-function "tabpage_get_windows" (("Tabpage" "tabpage"))
                     "ArrayOf(Window)" T NIL)
(mdata->lisp-function "tabpage_get_var" (("Tabpage" "tabpage") ("String" "name"))
                     "Object" T NIL)
(mdata->lisp-function "tabpage_set_var"
                     (("Tabpage" "tabpage") ("String" "name") ("Object" "value"))
                     "Object" T T)
(mdata->lisp-function "tabpage_get_window" (("Tabpage" "tabpage")) "Window" T NIL)
(mdata->lisp-function "tabpage_is_valid" (("Tabpage" "tabpage")) "Boolean" NIL
                     NIL)
(mdata->lisp-function "buffer_line_count" (("Buffer" "buffer")) "Integer" T NIL)
(mdata->lisp-function "buffer_get_line" (("Buffer" "buffer") ("Integer" "index"))
                     "String" T NIL)
(mdata->lisp-function "buffer_set_line"
                     (("Buffer" "buffer") ("Integer" "index") ("String" "line"))
                     "void" T T)
(mdata->lisp-function "buffer_del_line" (("Buffer" "buffer") ("Integer" "index"))
                     "void" T T)
(mdata->lisp-function "buffer_get_line_slice"
                     (("Buffer" "buffer") ("Integer" "start") ("Integer" "end")
                                          ("Boolean" "include_start") ("Boolean" "include_end"))
                     "ArrayOf(String)" T NIL)
(mdata->lisp-function "buffer_set_line_slice"
                     (("Buffer" "buffer") ("Integer" "start") ("Integer" "end")
                                          ("Boolean" "include_start") ("Boolean" "include_end")
                                          ("ArrayOf(String)" "replacement"))
                     "void" T T)
(mdata->lisp-function "buffer_get_var" (("Buffer" "buffer") ("String" "name"))
                     "Object" T NIL)
(mdata->lisp-function "buffer_set_var"
                     (("Buffer" "buffer") ("String" "name") ("Object" "value"))
                     "Object" T T)
(mdata->lisp-function "buffer_get_option" (("Buffer" "buffer") ("String" "name"))
                     "Object" T NIL)
(mdata->lisp-function "buffer_set_option"
                     (("Buffer" "buffer") ("String" "name") ("Object" "value"))
                     "void" T T)
(mdata->lisp-function "buffer_get_number" (("Buffer" "buffer")) "Integer" T NIL)
(mdata->lisp-function "buffer_get_name" (("Buffer" "buffer")) "String" T NIL)
(mdata->lisp-function "buffer_set_name" (("Buffer" "buffer") ("String" "name"))
                     "void" T T)
(mdata->lisp-function "buffer_is_valid" (("Buffer" "buffer")) "Boolean" NIL NIL)
(mdata->lisp-function "buffer_insert"
                     (("Buffer" "buffer") ("Integer" "lnum")
                                          ("ArrayOf(String)" "lines"))
                     "void" T T)
(mdata->lisp-function "buffer_get_mark" (("Buffer" "buffer") ("String" "name"))
                     "ArrayOf(Integer, 2)" T NIL)
