(in-package #:cl-neovim)


(cl:defun register-repl (instance)
  "Register repl with the running neovim instance."
  (call/s instance "vim_call_function" "lisp#RegisterRepl" (list (client-id instance))))

(cl:defun register-repl-callback (instance spec)
  "Tell neovim about callback."
  (call/s instance "vim_call_function" "lisp#RegisterReplCallback" (list (client-id instance) spec)))
