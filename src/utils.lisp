(in-package #:cl-neovim)


(cl:defun plist->string-hash (plist)
  "Convert property list PLIST into hash table. Keys are transformed into
   lowercase strings."
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (map 'string #'char-downcase (symbol-name k)) hash) v)
          finally (return hash))))

(cl:defun symbol-name= (symbol1 symbol2)
  "Compare two symbols by their name."
  (and (symbolp symbol1) (symbolp symbol2)
       (string= (symbol-name symbol1) (symbol-name symbol2))))

(cl:defun zip (&rest lists)
  (apply #'mapcar #'list lists))

(cl:defun mklst (obj)
  (if (listp obj) obj (list obj)))

(cl:defun alist-stable-intersection (as bs)
  "Intersection between 2 association lists as and bs, where the order is the
   same as in as and the values are associations from as."
  (let ((bs-alist (make-alist bs)))
    (remove-if-not #'(lambda (a) (assoc (car (mklst a)) bs-alist :test #'symbol-name=)) as)))

(cl:defun make-alist (lst)
  "Wrap a list of symbols (or lists) to a list of lists with first element
   being symbol."
  (mapcar #'mklst lst))

(cl:defun symbol-concat (&rest symbols) 
  "Concatenate symbol names and return resulting symbol."
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))
