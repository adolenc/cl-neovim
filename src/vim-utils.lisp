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

(cl:defun setterp (name)
  "Is name a setter?"
  (member "set" (split-sequence #\_ name) :test #'string=))

(cl:defun predicatep (name)
  "Is name a predicate?"
  (member "is" (split-sequence #\_ name) :test #'string=))

(cl:defun clean-up-name (name &optional (modifiers '("vim" "get" "set" "is")))
  "Removes all substrings specified in modifiers from name."
  (let* ((components (split-sequence #\_ name))
         (main-components (remove-if #'(lambda (c) (member c modifiers :test #'string=)) components))
         (suffix (if (predicatep name) "_p" "")))
    (format nil "~{~A~^_~}~A" main-components suffix)))
