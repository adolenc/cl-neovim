(in-package #:cl-neovim)


(defvar *nvim-types* (mrpc:define-extension-types
                       '(0
                         Buffer
                         Window
                         Tabpage)))
(defvar *nvim-instance* NIL "Binds to the last connection to neovim")


(cl:defun connect (&rest args &key host port file)
  (let ((mrpc:*extended-types* *nvim-types*))
    (setf *nvim-instance* (apply #'make-instance 'mrpc:client args))))

(cl:defun send-command (command async &rest args)
  "Send nvim command to neovim socket and return the result."
  (let ((mrpc:*extended-types* *nvim-types*))
    (if async
      (apply #'mrpc:notify *nvim-instance* command args)
      (apply #'mrpc:request *nvim-instance* command args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defun parse-parameters (parameters)
    "Extract names from nvim api's metadata of arguments into a list of symbols."
    (cond ((listp parameters) (mapcar #'(lambda (arg) (vim-name->symbol (second arg))) parameters))
          ((stringp parameters) (list (vim-name->symbol parameters)))
          (t NIL))))

(defmacro mdata->lisp-function (&key name parameters &allow-other-keys)
  "Create and export functions from the parsed nvim's api."
  (let* ((parameters (parse-parameters parameters))
         (n (vim-name->symbol (if (member name *dangerous-names* :test #'string-equal) name (clean-up-name name))))
         (async-n (symbol-concat n '/a)))
    (if (setterp name)
      `(progn (cl:defun (setf ,n) (,@(last parameters) ,@(butlast parameters))
                (funcall #'send-command ,name NIL ,@parameters)) 
              (cl:defun (setf ,async-n) (,@(last parameters) ,@(butlast parameters))
                (funcall #'send-command ,name T ,@parameters))
              (export ',async-n :cl-neovim)
              (export ',n :cl-neovim))
      `(progn (cl:defun ,n ,parameters
                (funcall #'send-command ,name NIL ,@parameters))
              (cl:defun ,async-n ,parameters
                (funcall #'send-command ,name T ,@parameters))
              (export ',async-n :cl-neovim)
              (export ',n :cl-neovim)))))


;;;; Rest of file generated with `generate-api.lisp'.
(MDATA->LISP-FUNCTION :PARAMETERS (("Buffer" "buffer")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "Integer" :NAME
                      "buffer_line_count")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("Integer" "index"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "String"
                      :NAME "buffer_get_line")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("Integer" "index")
                                           ("String" "line"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "buffer_set_line")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("Integer" "index"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "buffer_del_line")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("Integer" "start")
                                           ("Integer" "end")
                                           ("Boolean" "include_start")
                                           ("Boolean" "include_end"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE
                      "ArrayOf(String)" :NAME
                      "buffer_get_line_slice")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("Integer" "start")
                                           ("Integer" "end")
                                           ("Boolean" "strict_indexing"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE
                      "ArrayOf(String)" :NAME "buffer_get_lines")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("Integer" "start")
                                           ("Integer" "end")
                                           ("Boolean" "include_start")
                                           ("Boolean" "include_end")
                                           ("ArrayOf(String)" "replacement"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "buffer_set_line_slice")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("Integer" "start")
                                           ("Integer" "end")
                                           ("Boolean" "strict_indexing")
                                           ("ArrayOf(String)" "replacement"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "buffer_set_lines")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("String" "name"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "Object"
                      :NAME "buffer_get_var")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("String" "name")
                                           ("Object" "value"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "Object"
                      :NAME "buffer_set_var")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("String" "name"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "Object"
                      :NAME "buffer_get_option")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("String" "name")
                                           ("Object" "value"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "buffer_set_option")
(MDATA->LISP-FUNCTION :PARAMETERS (("Buffer" "buffer")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "Integer" :NAME
                      "buffer_get_number")
(MDATA->LISP-FUNCTION :PARAMETERS (("Buffer" "buffer")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "String" :NAME
                      "buffer_get_name")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("String" "name"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "buffer_set_name")
(MDATA->LISP-FUNCTION :RETURN-TYPE "Boolean" :ASYNC NIL :NAME
                      "buffer_is_valid" :PARAMETERS
                      (("Buffer" "buffer")))
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("Integer" "lnum")
                                           ("ArrayOf(String)" "lines"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "buffer_insert")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("String" "name"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE
                      "ArrayOf(Integer, 2)" :NAME
                      "buffer_get_mark")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("Integer" "src_id")
                                           ("String" "hl_group") ("Integer" "line")
                                           ("Integer" "col_start")
                                           ("Integer" "col_end"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "Integer"
                      :NAME "buffer_add_highlight")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Buffer" "buffer") ("Integer" "src_id")
                                           ("Integer" "line_start")
                                           ("Integer" "line_end"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "buffer_clear_highlight")
(MDATA->LISP-FUNCTION :PARAMETERS (("Tabpage" "tabpage")) :ASYNC
                      NIL :CAN-FAIL T :RETURN-TYPE
                      "ArrayOf(Window)" :NAME
                      "tabpage_get_windows")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Tabpage" "tabpage") ("String" "name"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "Object"
                      :NAME "tabpage_get_var")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Tabpage" "tabpage") ("String" "name")
                                             ("Object" "value"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "Object"
                      :NAME "tabpage_set_var")
(MDATA->LISP-FUNCTION :PARAMETERS (("Tabpage" "tabpage")) :ASYNC
                      NIL :CAN-FAIL T :RETURN-TYPE "Window" :NAME
                      "tabpage_get_window")
(MDATA->LISP-FUNCTION :RETURN-TYPE "Boolean" :ASYNC NIL :NAME
                      "tabpage_is_valid" :PARAMETERS
                      (("Tabpage" "tabpage")))
(MDATA->LISP-FUNCTION :PARAMETERS (("String" "str")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "void" :NAME
                      "vim_command")
(MDATA->LISP-FUNCTION :RETURN-TYPE "void" :ASYNC NIL :NAME
                      "vim_feedkeys" :PARAMETERS
                      (("String" "keys") ("String" "mode")
                                         ("Boolean" "escape_csi")))
(MDATA->LISP-FUNCTION :RETURN-TYPE "Integer" :ASYNC T :NAME
                      "vim_input" :PARAMETERS (("String" "keys")))
(MDATA->LISP-FUNCTION :RETURN-TYPE "String" :ASYNC NIL :NAME
                      "vim_replace_termcodes" :PARAMETERS
                      (("String" "str") ("Boolean" "from_part")
                                        ("Boolean" "do_lt") ("Boolean" "special")))
(MDATA->LISP-FUNCTION :PARAMETERS (("String" "str")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "String" :NAME
                      "vim_command_output")
(MDATA->LISP-FUNCTION :PARAMETERS (("String" "str")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "Object" :NAME
                      "vim_eval")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("String" "fname") ("Array" "args")) :ASYNC
                      NIL :CAN-FAIL T :RETURN-TYPE "Object" :NAME
                      "vim_call_function")
(MDATA->LISP-FUNCTION :PARAMETERS (("String" "str")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "Integer" :NAME
                      "vim_strwidth")
(MDATA->LISP-FUNCTION :RETURN-TYPE "ArrayOf(String)" :ASYNC NIL
                      :NAME "vim_list_runtime_paths" :PARAMETERS
                      NIL)
(MDATA->LISP-FUNCTION :PARAMETERS (("String" "dir")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "void" :NAME
                      "vim_change_directory")
(MDATA->LISP-FUNCTION :PARAMETERS NIL :ASYNC NIL :CAN-FAIL T
                      :RETURN-TYPE "String" :NAME
                      "vim_get_current_line")
(MDATA->LISP-FUNCTION :PARAMETERS (("String" "line")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "void" :NAME
                      "vim_set_current_line")
(MDATA->LISP-FUNCTION :PARAMETERS NIL :ASYNC NIL :CAN-FAIL T
                      :RETURN-TYPE "void" :NAME
                      "vim_del_current_line")
(MDATA->LISP-FUNCTION :PARAMETERS (("String" "name")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "Object" :NAME
                      "vim_get_var")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("String" "name") ("Object" "value")) :ASYNC
                      NIL :CAN-FAIL T :RETURN-TYPE "Object" :NAME
                      "vim_set_var")
(MDATA->LISP-FUNCTION :PARAMETERS (("String" "name")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "Object" :NAME
                      "vim_get_vvar")
(MDATA->LISP-FUNCTION :PARAMETERS (("String" "name")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "Object" :NAME
                      "vim_get_option")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("String" "name") ("Object" "value")) :ASYNC
                      NIL :CAN-FAIL T :RETURN-TYPE "void" :NAME
                      "vim_set_option")
(MDATA->LISP-FUNCTION :RETURN-TYPE "void" :ASYNC NIL :NAME
                      "vim_out_write" :PARAMETERS
                      (("String" "str")))
(MDATA->LISP-FUNCTION :RETURN-TYPE "void" :ASYNC NIL :NAME
                      "vim_err_write" :PARAMETERS
                      (("String" "str")))
(MDATA->LISP-FUNCTION :RETURN-TYPE "void" :ASYNC NIL :NAME
                      "vim_report_error" :PARAMETERS
                      (("String" "str")))
(MDATA->LISP-FUNCTION :RETURN-TYPE "ArrayOf(Buffer)" :ASYNC NIL
                      :NAME "vim_get_buffers" :PARAMETERS NIL)
(MDATA->LISP-FUNCTION :RETURN-TYPE "Buffer" :ASYNC NIL :NAME
                      "vim_get_current_buffer" :PARAMETERS NIL)
(MDATA->LISP-FUNCTION :PARAMETERS (("Buffer" "buffer")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "void" :NAME
                      "vim_set_current_buffer")
(MDATA->LISP-FUNCTION :RETURN-TYPE "ArrayOf(Window)" :ASYNC NIL
                      :NAME "vim_get_windows" :PARAMETERS NIL)
(MDATA->LISP-FUNCTION :RETURN-TYPE "Window" :ASYNC NIL :NAME
                      "vim_get_current_window" :PARAMETERS NIL)
(MDATA->LISP-FUNCTION :PARAMETERS (("Window" "window")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "void" :NAME
                      "vim_set_current_window")
(MDATA->LISP-FUNCTION :RETURN-TYPE "ArrayOf(Tabpage)" :ASYNC NIL
                      :NAME "vim_get_tabpages" :PARAMETERS NIL)
(MDATA->LISP-FUNCTION :RETURN-TYPE "Tabpage" :ASYNC NIL :NAME
                      "vim_get_current_tabpage" :PARAMETERS NIL)
(MDATA->LISP-FUNCTION :PARAMETERS (("Tabpage" "tabpage")) :ASYNC
                      NIL :CAN-FAIL T :RETURN-TYPE "void" :NAME
                      "vim_set_current_tabpage")
(MDATA->LISP-FUNCTION :PARAMETERS (("String" "event")) :ASYNC NIL
                      :RETURN-TYPE "void" :RECEIVES-CHANNEL-ID T
                      :NAME "vim_subscribe")
(MDATA->LISP-FUNCTION :PARAMETERS (("String" "event")) :ASYNC NIL
                      :RETURN-TYPE "void" :RECEIVES-CHANNEL-ID T
                      :NAME "vim_unsubscribe")
(MDATA->LISP-FUNCTION :RETURN-TYPE "Integer" :ASYNC NIL :NAME
                      "vim_name_to_color" :PARAMETERS
                      (("String" "name")))
(MDATA->LISP-FUNCTION :RETURN-TYPE "Dictionary" :ASYNC NIL :NAME
                      "vim_get_color_map" :PARAMETERS NIL)
(MDATA->LISP-FUNCTION :PARAMETERS NIL :ASYNC T :RETURN-TYPE "Array"
                      :RECEIVES-CHANNEL-ID T :NAME
                      "vim_get_api_info")
(MDATA->LISP-FUNCTION :PARAMETERS (("Window" "window")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "Buffer" :NAME
                      "window_get_buffer")
(MDATA->LISP-FUNCTION :PARAMETERS (("Window" "window")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE
                      "ArrayOf(Integer, 2)" :NAME
                      "window_get_cursor")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Window" "window")
                       ("ArrayOf(Integer, 2)" "pos"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "window_set_cursor")
(MDATA->LISP-FUNCTION :PARAMETERS (("Window" "window")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "Integer" :NAME
                      "window_get_height")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Window" "window") ("Integer" "height"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "window_set_height")
(MDATA->LISP-FUNCTION :PARAMETERS (("Window" "window")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "Integer" :NAME
                      "window_get_width")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Window" "window") ("Integer" "width"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "window_set_width")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Window" "window") ("String" "name"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "Object"
                      :NAME "window_get_var")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Window" "window") ("String" "name")
                                           ("Object" "value"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "Object"
                      :NAME "window_set_var")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Window" "window") ("String" "name"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "Object"
                      :NAME "window_get_option")
(MDATA->LISP-FUNCTION :PARAMETERS
                      (("Window" "window") ("String" "name")
                                           ("Object" "value"))
                      :ASYNC NIL :CAN-FAIL T :RETURN-TYPE "void"
                      :NAME "window_set_option")
(MDATA->LISP-FUNCTION :PARAMETERS (("Window" "window")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE
                      "ArrayOf(Integer, 2)" :NAME
                      "window_get_position")
(MDATA->LISP-FUNCTION :PARAMETERS (("Window" "window")) :ASYNC NIL
                      :CAN-FAIL T :RETURN-TYPE "Tabpage" :NAME
                      "window_get_tabpage")
(MDATA->LISP-FUNCTION :RETURN-TYPE "Boolean" :ASYNC NIL :NAME
                      "window_is_valid" :PARAMETERS
                      (("Window" "window")))
