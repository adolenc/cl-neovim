(in-package #:cl-neovim)


(cl:defun function-metadata (f)
  (loop for k being the hash-keys in f using (hash-value v)
        append (list (alexandria:make-keyword (vim-name->symbol k)) v)))

(cl:defun parse-api (api)
  (let ((functions (gethash "functions" api)))
    (loop for f in functions
          collect (function-metadata f))))

(cl:defun retrieve-api ()
  (second (send-command "vim_get_api_info" NIL)))

(cl:defun generate-api ()
  (let ((api (parse-api (retrieve-api))))
    `(progn ,@(mapcar #'(lambda (metadata) `(mdata->lisp-function ,@metadata)) api))))
