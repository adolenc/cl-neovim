(in-package #:cl-neovim)


(cl:defun vim-name->symbol (str)
  "Convert string into symbol."
  (intern (substitute #\- #\_ (format nil "~:@(~A~)" str)))) 

(cl:defun symbol->vim-name (lisp-name)
  "Convert lisp symbol into vim name. Basically turns hyphen-separated name
   into camelcase string."
  (let* ((str (symbol-name lisp-name))
         (parts (split-sequence:split-sequence #\- str)))
    (format nil "~{~:(~A~)~^~}" parts)))
