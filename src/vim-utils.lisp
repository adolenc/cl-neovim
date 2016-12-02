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
  (and (member "set" (split-sequence #\_ name) :test #'string=) T))

(cl:defun predicatep (name)
  "Is name a predicate?"
  (and (member "is" (split-sequence #\_ name) :test #'string=) T))

(cl:defun clean-up-name (name &optional
                              (modifiers '("vim" "nvim" "get" "set" "is" "list"))
                              (replacements '(("buf" "buffer") ("win" "window") ("bufs" "buffers") ("wins" "windows"))))
  "Removes all substrings specified in modifiers from name and applies all
   replacements."
  (let ((components (split-sequence #\_ name)))
    (setf components (remove-if #'(lambda (c) (member c modifiers :test #'string=)) components))
    (loop for (old new) in replacements
          do (setf components (substitute new old components :test #'string=)))
    (if (predicatep name)
      (setf components (append components '("p"))))
    (format nil "~{~A~^_~}" components)))
