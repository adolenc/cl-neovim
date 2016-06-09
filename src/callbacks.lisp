(in-package #:cl-neovim)


(cl:defun short-names (args)
  "Return short names for &opts arguments"
  (mapcar #'(lambda (arg) (if (and (not (second arg))
                                   (or (symbol-name= (first arg) 'args)
                                       (symbol-name= (first arg) 'nargs)))
                            NIL
                            (or (second arg) (first arg))))
          (make-alist args)))

(cl:defun generate-arglist (args args-opts declare-opts nvim-opts)
  "Generate argument list that will properly parse neovim's format of passed
   arguments. "
  (if (not (listp nvim-opts))
    (fill-args-into-opts args declare-opts nvim-opts) 
    (let* ((args-opts (append '((nargs) (args)) (mapcar #'mklst args-opts)))
           (nvim-opts (fill-args-into-opts args declare-opts nvim-opts)) 
           (ordered-opts (alist-stable-intersection (mapcar #'mklst nvim-opts) (append '((nargs) (args)) declare-opts)))
           (ignored-opts (mapcar #'(lambda (arg)
                                     (if (> (length arg) 1)
                                       arg
                                       (or (assoc (car arg) args-opts :test #'symbol-name=) (gensym))))
                                 ordered-opts))
           (short-arg-names (short-names ignored-opts)))
      short-arg-names)))

(cl:defun fill-declare-opts (declare-opts)
  "If user specified just '[opt] in declare opts, fill it out with the default
   value. Don't modify the opts that were defined along with their values
   '([opt] [val])."
  (let ((defaults '((nargs "*") (complete "") (range "") (count "") (bang "") (bar "") (register ""))))
    (mapcar #'(lambda (opt) (if (listp opt)
                              opt
                              (assoc opt defaults :test #'symbol-name=)))
            declare-opts)))

(cl:defun fill-args-into-opts (args declare-opts nvim-opts) 
  "Based on the value of nargs declaration either fill in and replace nargs
   with actual user expected arguments, or ignore them, and replace args with
   actual user arguments."
  (if (listp nvim-opts)
    (let* ((nargs (second (assoc 'nargs declare-opts :test #'symbol-name=)))
           (opts-wo-nargs (if (or (not nargs) (string= "0" nargs))
                            (remove 'nargs nvim-opts :test #'symbol-name=)
                            (substitute `(nargs ,args) 'nargs nvim-opts :test #'symbol-name=))))
      (substitute `(args ,args) 'args opts-wo-nargs :test #'symbol-name=))
    (subst args 'args nvim-opts :test #'symbol-name=)))

(cl:defun generate-specs (declare-opts type)
  "Generate the specs from declare opts user specified."
  (let* ((opts (mapcar #'(lambda (l) (list (intern (symbol-name (first l)) 'keyword) (rest l))) declare-opts))
         (opts (alexandria:flatten (alexandria:alist-plist opts)))
         (opts (if (and (string= type "autocmd") (not (getf opts :pattern)))
                 (append '(:pattern "*") opts)
                 opts)))
     (substitute :eval :vim-eval opts)))

(cl:defun generate-callback-name (type name spec-opts)
  "Generate the callback name neovim will use when referring to this
   function/command/autocmd."
  (if *path*
    (concatenate 'string
                 (format nil "~A:~A:~A" *path* type name)
                 (if (string= type "autocmd") (format nil ":~A" (getf spec-opts :pattern)) ""))
    name))

(defmacro redirect-output (&body body)
  `(let ((*standard-output* *debug-stream*)
         (*error-output* *debug-stream*))
     ,@body))

(cl:defun construct-callback (type sync nvim-opts name args-and-opts body)
  "Construct the callback, register it with proper name, and generate specs
   based on the arguments passed."
  (let ((fake-lambda-form `(defun ,name ,args-and-opts ,@body)))
    (form-fiddle:with-destructured-lambda-form (:docstring docstring :declarations declarations :forms forms) fake-lambda-form
      (destructuring-bind (&optional args arglist-opts) (split-sequence:split-sequence '&opts args-and-opts :test #'symbol-name=)
        (let* ((name (if (stringp name) name (symbol->vim-name name)))
               (raw-declare-opts (rest (assoc 'opts (cdar declarations) :test #'symbol-name=)))
               (declare-opts (fill-declare-opts raw-declare-opts))
               (not-a-host-p (or (not (boundp *using-host*))
                                 (and (boundp *using-host*) (not *using-host*))))
               (arglist (generate-arglist args arglist-opts declare-opts nvim-opts))
               (spec-opts (generate-specs declare-opts type))
               (callback-name (generate-callback-name type name spec-opts))
               (r (gensym)))
          `(progn
             (push (plist->string-hash (list :sync ,(if sync 1 0)
                                             :name ,name
                                             :type ,type
                                             :opts (plist->string-hash ',spec-opts)))
                   *specs*)
             (mrpc:register-callback
               *nvim-instance*
               ,callback-name
               #'(lambda ,(if not-a-host-p args-and-opts `(&rest ,r))
                   ,docstring
                   (destructuring-bind ,@(if not-a-host-p '(() ()) `(,arglist ,r))
                     (redirect-output
                       ,@forms))))))))))

(defmacro defcommand (name args &body body)
  ; nvim-options for command found in runtime/autoload/remote/define.vim#L54-L87
  (construct-callback "command" NIL '(nargs range count bang register vim-eval) name args body))

(defmacro defcommand/s (name args &body body)
  ; nvim-options for command found in runtime/autoload/remote/define.vim#L54-L87
  (construct-callback "command" T '(nargs range count bang register vim-eval) name args body))


(defmacro defautocmd (name args &body body)
  ; nvim-options for autocmd found in runtime/autoload/remote/define.vim#L121-L128
  (construct-callback "autocmd" NIL 'args name args body))

(defmacro defautocmd/s (name args &body body)
  ; nvim-options for autocmd found in runtime/autoload/remote/define.vim#L121-L128
  (construct-callback "autocmd" T 'args name args body))


(defmacro defun (name args &body body)
  ; nvim-options for function found in runtime/autoload/remote/define.vim#L158-L166
  (construct-callback "function" NIL '(args vim-eval) name args body))

(defmacro defun/s (name args &body body)
  ; nvim-options for function found in runtime/autoload/remote/define.vim#L158-L166
  (construct-callback "function" T '(args vim-eval) name args body))
