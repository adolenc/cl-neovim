(ql:quickload :cl-neovim-tests :silent T)
(in-package :cl-neovim-tests)


(nvim:defcommand lisp-host-run-tests :sync ()
   (with-open-file (*standard-output* "/tmp/out.log" :direction :output :if-does-not-exist :create :if-exists :supersede)
     (fiveam:run! 'neovim-test-suite)
     (hmmm)))
