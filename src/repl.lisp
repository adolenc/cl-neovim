(in-package #:cl-neovim)


(cl:defun register-repl (instance)
  "Register repl with the running neovim instance."
  (unless (gethash "__IsActiveLispRepl__" (mrpc::callbacks instance))
    (mrpc:register-callback instance "__IsActiveLispRepl__" #'(lambda () T)))
  (call/s "vim_call_function" "lisp#RegisterRepl" #()))

(cl:defun register-repl-callback (instance spec)
  "Tell neovim about callback."
  (declare (ignore instance))
  (call/s "vim_call_function" "lisp#RegisterReplCallback" (list spec)))
