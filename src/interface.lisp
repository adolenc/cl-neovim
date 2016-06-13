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

(cl:defun call/s (command &rest args)
  "Send nvim command to neovim socket and return the result."
  (let ((mrpc:*extended-types* *nvim-types*))
    (apply #'mrpc:request *nvim-instance* command args)))

(cl:defun call/a (command &rest args)
  "Send nvim command to neovim socket asynchronously, returning the control
back to the caller immediately and discarding all return values/errors."
  (let ((mrpc:*extended-types* *nvim-types*))
    (apply #'mrpc:notify *nvim-instance* command args)))

(cl:defun parse-parameters (parameters)
  "Extract names from nvim api's metadata of arguments into a list of symbols."
  (cond ((listp parameters) (mapcar #'(lambda (arg) (vim-name->symbol (second arg))) parameters))
        ((stringp parameters) (list (vim-name->symbol parameters)))
        (t NIL)))

(defmacro mdata->lisp-function (&key name parameters &allow-other-keys)
  "Create and export functions from the parsed nvim's api."
  (let* ((parameters (parse-parameters parameters))
         (fn-name (vim-name->symbol (if (member name *dangerous-names* :test #'string-equal) name (clean-up-name name))))
         (sync-fn-name (symbol-concat fn-name '/s))
         (async-fn-name (symbol-concat fn-name '/a))
         (funcalls `((,fn-name #'call/s)
                     (,sync-fn-name #'call/s)
                     (,async-fn-name #'call/a))))
    `(progn ,@(loop for (fn-name fn) in funcalls
                    collect (if (setterp name)
                              `(cl:defun (setf ,fn-name) (,@(last parameters) ,@(butlast parameters))
                                 (funcall ,fn ,name ,@parameters))
                              `(cl:defun ,fn-name ,parameters
                                 (funcall ,fn ,name ,@parameters))))
            ,@(loop for (fn-name _) in funcalls
                    collect `(export ',fn-name :cl-neovim)))))
