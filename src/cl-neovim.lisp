(in-package #:cl-neovim)

(defparameter *debug* "/tmp/s.log")

(defparameter *specs* NIL "A list of all the specs nvim needs.")
(defparameter *path* NIL "A list of all the specs nvim needs.")
(defparameter *nvim-types* (mrpc:define-extension-types
                             '(0
                               Buffer
                               Window
                               Tabpage)))

(cl:defun dbg (str &rest args)
  (if *debug*
    (with-open-file (s *debug* :direction :output :if-exists :append :if-does-not-exist :create)
      (apply #'format s str args))))

(cl:defun plist->hash (plist)
  "Convert property list plist into hash table. Keys are transformed into
   lowercase strings."
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (map 'string #'char-downcase (symbol-name k)) hash) v)
          finally (return hash))))

(cl:defun lisp->vim-name (lisp-name)
  "Convert lisp symbol into vim name. Basically turns hyphen-separated name
   into camelcase string."
  (let* ((str (symbol-name lisp-name))
         (parts (split-sequence:split-sequence #\- str)))
    (format nil "~{~:(~A~)~^~}" parts)))

(cl:defun symbol-name= (symbol1 symbol2)
  "Compare two symbols by their name."
  (and (symbolp symbol1) (symbolp symbol2)
       (string= (symbol-name symbol1) (symbol-name symbol2))))

(cl:defun mklst (obj) (if (listp obj) obj (list obj)))

(cl:defun alist-stable-intersection (as bs)
  "Intersection between 2 association lists as and bs, where the order is the
   same as in as and the values are associations from as."
  (let ((bs-alist (make-alist bs)))
    (remove-if-not #'(lambda (a) (assoc (car (mklst a)) bs-alist :test #'symbol-name=)) as)))

(cl:defun make-alist (lst)
  "Wrap a list of symbols (or lists) to a list of lists with first element
   being symbol."
  (mapcar #'mklst lst))

(cl:defun short-names (args)
  "Return short names for &opts arguments"
  (mapcar #'(lambda (arg) (or (second arg) (first arg)))
          (make-alist args)))

(cl:defun construct-arglist-opts (user-arg-opts user-declare-opts nvim-opts)
  (let* ((user-arg-opts (mapcar #'mklst user-arg-opts))
         (ordered-opts (alist-stable-intersection (make-alist nvim-opts) user-declare-opts))
         (ignored-opts (mapcar #'(lambda (arg) (or (assoc arg user-arg-opts :test #'symbol-name=) (gensym))) (mapcar #'car ordered-opts)))
         (short-arg-names (short-names ignored-opts)))
    short-arg-names))

(cl:defun construct-callback (type nvim-opts name-args-decls-body)
  (destructuring-bind (fun name qualifiers args-and-opts docstring decls body) (form-fiddle:split-lambda-form (cons 'defun name-args-decls-body))
    (declare (ignore fun))
    (destructuring-bind (&optional args arglist-opts) (split-sequence:split-sequence '&opts args-and-opts :test #'symbol-name=)
      (let* ((name (if (stringp name) name (lisp->vim-name name)))
             (sync (member :sync qualifiers))
             (declare-opts (rest (assoc 'opts (cdar decls) :test #'symbol-name=)))
             (arglist-opts (construct-arglist-opts arglist-opts declare-opts nvim-opts))
             (opts (mapcar #'(lambda (l) (list (intern (symbol-name (first l)) 'keyword) (rest l))) declare-opts))
             (opts (alexandria:flatten (alexandria:alist-plist opts)))
             (opts (if (and (string= type "autocmd") (not (getf opts :pattern)))
                     (append '(:pattern "*") opts)
                     opts))
             (callback-name (if *path*
                              (concatenate 'string
                                           (format nil "~A:~A:~A" *path* type name)
                                           (if (string= type "autocmd") (format nil ":~A" (getf opts :pattern)) ""))
                              name))
             (a (gensym)))
        `(progn
           (push (plist->hash (list :sync ,(if sync 1 0)
                                    :name ,name
                                    :type ,type
                                    :opts (plist->hash ',opts)))
                 *specs*)
           (,(if sync 'mrpc:register-request-callback 'mrpc:register-notification-callback)
             ,callback-name
             #'(lambda (,a ,@arglist-opts)
                 ,docstring
                 (destructuring-bind ,args (mklst ,a)
                   ,@body))))))))

(defmacro defcommand (&rest name-args-decls-body)
  (construct-callback "command" '(range count bang register) name-args-decls-body))

(defmacro defautocmd (&rest name-args-decls-body)
  (construct-callback "autocmd" '() name-args-decls-body))

(defmacro defun (&rest name-args-decls-body)
  (construct-callback "function" '() name-args-decls-body))

(cl:defun send-command (command async &rest args)
  "Send nvim command to neovim socket and return the result."
  (let ((mrpc:*extended-types* *nvim-types*))
    (mrpc:request command args async)))

(cl:defun connect (&rest args &key host port file)
  (let ((mrpc:*extended-types* *nvim-types*))
    (apply #'mrpc:connect args)))
