(ql:quickload :cl-neovim-tests :silent T)
(in-package :cl-neovim-tests)


(nvim:defcommand lisp-host-run-tests :sync ()
   (let ((*standard-output* nvim:*debug-stream*)
         (*error-output* nvim:*debug-stream*))
     (fiveam:run! 'neovim-test-suite)
     (hmmm)))
