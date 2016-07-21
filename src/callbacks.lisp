(in-package #:cl-neovim)


(defvar +opts-conversions+
  '((bang . (= bang 1))))

(cl:defun short-names (args)
  "Return short names for &opts arguments"
  (mapcar #'(lambda (arg)
              (if (and (not (second arg))
                       (or (symbol-name= (first arg) 'args)
                           (symbol-name= (first arg) 'nargs)))
                NIL
                (or (second arg) (first arg))))
          (make-alist args)))

(cl:defun generate-arglist (args args-opts declare-opts nvim-opts)
  "Generate argument list that will properly parse neovim's format of passed
   arguments. "
  (if (not (listp nvim-opts))
    (values (fill-args-into-opts args declare-opts nvim-opts) '())
    (let* ((args-opts (append '((nargs) (args)) (mapcar #'mklst args-opts)))
           (nvim-opts (fill-args-into-opts args declare-opts nvim-opts))
           (ordered-opts (alist-stable-intersection (mapcar #'mklst nvim-opts) (append '((nargs) (args)) declare-opts)))
           (placeholder-gensyms)
           (ignored-opts (mapcar #'(lambda (arg)
                                     (if (> (length arg) 1)
                                       arg
                                       (or (assoc (car arg) args-opts :test #'symbol-name=) (car (push (gensym) placeholder-gensyms)))))
                                 ordered-opts))
           (short-arg-names (short-names ignored-opts)))
      (values short-arg-names placeholder-gensyms))))

(cl:defun fill-declare-opts (declare-opts &key (default-nargs "*"))
  "If user specified just '[opt] in declare opts, fill it out with the default
   value. Don't modify the opts that were defined along with their values
   '([opt] [val])."
  (let ((defaults `((nargs ,default-nargs) (complete "") (range "") (count "") (bang "") (bar "") (register "") (pattern "*"))))
    (mapcar #'(lambda (opt)
                (if (listp opt)
                  opt
                  (assoc opt defaults :test #'symbol-name=)))
            declare-opts)))

(cl:defun fill-args-into-opts (args declare-opts nvim-opts)
  "Based on the value of nargs declaration either fill in and replace nargs
   with actual user expected arguments, or ignore them, and replace args with
   actual user arguments."
  (if (listp nvim-opts)
    (if (find 'nargs nvim-opts :test #'symbol-name=)
      (substitute `(nargs ,args) 'nargs nvim-opts :test #'symbol-name=)
      (substitute `(args  ,args) 'args  nvim-opts :test #'symbol-name=))
    args))

(cl:defun calculate-nargs (lambda-list)
  (cond ((null lambda-list) "0")
        ((= (length lambda-list) 1) "1")
        ((and (= (length lambda-list) 2) (symbol-name= (first lambda-list) '&optional)) "?")
        ((and (not (symbol-name= (first lambda-list) '&rest)) (position '&rest lambda-list :test #'symbol-name=)) "+")
        (t "*")))

(cl:defun generate-specs (declare-opts type)
  "Generate the specs from declare opts user specified."
  (let ((opts (mapcar #'(lambda (l) (list (intern (symbol-name (first l)) 'keyword) (rest l))) declare-opts)))
    (alexandria:flatten (alexandria:alist-plist opts))))

(cl:defun append-arglist-opts (declare-opts arglist-opts)
  (let ((arglist-opts-names (mapcar #'(lambda (o) (or (and (listp o) (first o)) o)) arglist-opts)))
    (remove-duplicates (append declare-opts arglist-opts-names)
                       :test #'symbol-name=
                       :from-end T
                       :key #'(lambda (o) (or (and (listp o) (first o)) o)))))

(cl:defun generate-callback-name (type name spec-opts)
  "Generate the callback name neovim will use when referring to this
   function/command/autocmd."
  (format nil "~A:~A:~A~A" *path* type name (if (string= type "autocmd")
                                              (format nil ":~A" (getf spec-opts :pattern))
                                              "")))

(cl:defun make-spec (&key sync name type opts)
  (plist->string-hash (list :sync (or sync :false)
                            :name name
                            :type type
                            :opts (plist->string-hash opts))))

(cl:defun extract-opts-declaration (declarations)
  (flet ((opts-declaration-specifier-p (declaration-specifier)
           (symbol-name= 'opts (car declaration-specifier))))
    (let* ((declaration-specifiers (alexandria:mappend #'cdr declarations))
           (opts-declaration (find-if #'opts-declaration-specifier-p declaration-specifiers))
           (other-declarations (remove-if #'opts-declaration-specifier-p declaration-specifiers)))
      (values (rest opts-declaration) other-declarations))))

(defmacro redirect-output ((&optional (where *log-stream*)) &body body)
  `(let ((*standard-output* ,where)
         (*trace-output* ,where)
         (*error-output* ,where)
         (*debug-io* ,where)
         (*query-io* ,where))
     ,@body))

(cl:defun ignored-variables (declarations)
  (let ((ignore-declarations (remove-if-not #'(lambda (d)
                                                (symbol-name= (car d) 'ignore))
                                            declarations)))
    (alexandria:mappend #'cdr ignore-declarations)))


(cl:defun construct-callback (type sync nvim-opts required-opts name args-and-opts body)
  "Construct the callback, register it with proper name, and generate specs
   based on the arguments passed."
  (let ((fake-lambda-form `(defun ,name ,args-and-opts ,@body)))
    (form-fiddle:with-destructured-lambda-form (:docstring docstring :declarations declarations :forms forms) fake-lambda-form
      (multiple-value-bind (declare-opts other-declarations) (extract-opts-declaration declarations)
        (destructuring-bind (&optional args arglist-opts) (split-sequence:split-sequence '&opts args-and-opts :test #'symbol-name=)
          (let* ((spec-name (if (stringp name) name (symbol->vim-name name)))
                 (return-name (if (stringp name) (string-upcase name) name))
                 (declare-opts (append declare-opts required-opts))
                 (declare-opts (fill-declare-opts (append-arglist-opts declare-opts arglist-opts) :default-nargs (calculate-nargs args)))
                 (spec-opts (generate-specs declare-opts type))
                 (return-symbol (intern (if (stringp name) return-name (symbol-name return-name))))
                 (ignored-args (ignored-variables other-declarations))
                 (conversions (loop with opt-name and short-name and conversion and ignored
                                    for opt in arglist-opts
                                    do (setf opt-name   (or (and (listp opt) (first opt)) opt)
                                             short-name (or (and (listp opt) (second opt)) opt)
                                             conversion (assoc opt-name +opts-conversions+ :test #'symbol-name=)
                                             ignored (find short-name ignored-args :test #'symbol-name=))
                                    when (and conversion (not ignored))
                                      collect `(,short-name ,(subst short-name opt-name (rest conversion) :test #'symbol-name=)))))
            (multiple-value-bind (arglist placeholder-gensyms) (generate-arglist args arglist-opts declare-opts nvim-opts)
              (let ((new-declarations (append other-declarations (if placeholder-gensyms `((ignorable ,@placeholder-gensyms))))))
                (alexandria:with-gensyms (callback-name spec r)
                  `(eval-when (:compile-toplevel :load-toplevel :execute)
                     (let ((,spec (make-spec :sync ,sync
                                             :name ,spec-name
                                             :type ,type
                                             :opts ',spec-opts))
                           (,callback-name (generate-callback-name ,type ,spec-name ',spec-opts)))
                       (push ,spec *specs*)
                       (unless (and (boundp *using-host*) *using-host*)
                         (progn (register-repl *nvim-instance*)
                                (register-repl-callback *nvim-instance* ,spec)))
                       (mrpc:register-callback
                         *nvim-instance*
                         ,callback-name
                         #'(lambda (&rest ,r)
                             ,docstring
                             (block ,return-symbol
                                    (destructuring-bind ,arglist ,r
                                      ,(if new-declarations
                                         `(declare ,@new-declarations))
                                      (let (,@conversions)
                                        (redirect-output (*log-stream*)
                                          ,@forms)))))))))))))))))

(defmacro defcommand (name args &body body)
  ; nvim-options for command found in runtime/autoload/remote/define.vim#L54-L87
  (construct-callback "command" NIL '(nargs range count bang register eval) '(nargs) name args body))

(defmacro defcommand/s (name args &body body)
  ; nvim-options for command found in runtime/autoload/remote/define.vim#L54-L87
  (construct-callback "command" T '(nargs range count bang register eval) '(nargs) name args body))


(defmacro defautocmd (name args &body body)
  ; nvim-options for autocmd found in runtime/autoload/remote/define.vim#L121-L128
  (construct-callback "autocmd" NIL 'args '(pattern) name args body))

(defmacro defautocmd/s (name args &body body)
  ; nvim-options for autocmd found in runtime/autoload/remote/define.vim#L121-L128
  (construct-callback "autocmd" T 'args '(pattern) name args body))


(defmacro defun (name args &body body)
  ; nvim-options for function found in runtime/autoload/remote/define.vim#L158-L166
  (construct-callback "function" NIL '(args eval) '() name args body))

(defmacro defun/s (name args &body body)
  ; nvim-options for function found in runtime/autoload/remote/define.vim#L158-L166
  (construct-callback "function" T '(args eval) '() name args body))
