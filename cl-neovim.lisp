(in-package #:cl-neovim)

(defparameter *specs* NIL "A list of all the specs nvim needs.")
(defparameter *path* NIL "A list of all the specs nvim needs.")
(defparameter *nvim-types* (mrpc:define-extension-types
                             '(0
                               Buffer
                               Window
                               Tabpage)))

(defun plist->hash (plist)
  "Convert property list plist into hash table. Keys are transformed into
   lowercase strings."
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (map 'string #'char-downcase (symbol-name k)) hash) v)
          finally (return hash))))

(defun lisp->vim-name (lisp-name)
  "Convert lisp symbol into vim name. Basically turns hyphen-separated name
   into camelcase string."
  (let* ((str (symbol-name lisp-name))
         (parts (split-sequence #\- str)))
    (format nil "~{~:(~A~)~^~}" parts)))

(defun parse-body (body)
  (if (and (listp (car body)) (equal (symbol-name (caar body)) (symbol-name 'opts)))
    (values (cadar body) (rest body))
    (values NIL body)))

(defun define-callback (type name args sync opts body)
  (let ((name (if (stringp name) name (lisp->vim-name name))))
    `(progn (push (plist->hash (list :sync ,(if sync 1 0)
                                     :name ,name
                                     :type ,type
                                     :opts (plist->hash ',opts)))
                  *specs*)
            (,(if sync 'mrpc:register-request-callback 'mrpc:register-notification-callback)
              ,(if *path*
                 (concatenate 'string
                              (format nil "~A:~A:~A" *path* type name)
                              (if (string= type "autocmd") (format nil ":~A" (getf opts :pattern)) ""))
                 name)
              #'(lambda ,args ,@body)))))

(defun construct-callback (type name args)
  (cond ((or (listp (first args)))
         (multiple-value-bind (opts body) (parse-body (rest args))
           (define-callback type name (first args) NIL opts body)))
         ((eq (first args) :sync)
          (multiple-value-bind (opts body) (parse-body (nthcdr 2 args))
            (define-callback type name (second args) T opts body)))
         ((eq (first args) :async)
          (multiple-value-bind (opts body) (parse-body (nthcdr 2 args))
            (define-callback type name (second args) NIL opts body)))))


(defmacro defcmd (name &rest args)
  (construct-callback "command" name args))

(defmacro defautocmd (name &rest args)
  (construct-callback "autocmd" name args))

(defmacro defn (name &rest args)
  (construct-callback "function" name args))

(defun send-command (command async &rest args)
  "Send nvim command to neovim socket and return the result."
  (let ((mrpc:*extended-types* *nvim-types*))
    (mrpc:request command args async)))


(defun connect (&rest args &key host port filename)
  (let ((mrpc:*extended-types* *nvim-types*))
    (apply #'mrpc:run args)))
