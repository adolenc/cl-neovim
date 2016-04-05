(asdf:defsystem #:cl-neovim-tests
  :description "Tests for cl-neovim"
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:cl-neovim
               #:fiveam)
  :serial t
  :components ((:file "t/package")
               (:file "t/common"))
  :perform (test-op (op c)
             ; TODO: FIX, does not work.
             ; (funcall (intern (symbol-name :connect) (find-package :cl-neovim)) :file "/tmp/nvim")
             ; (funcall (intern (symbol-name :run!) (find-package :fiveam))
             ;          (intern (symbol-name :neovim-test-suite) (find-package :fiveam)))
             ))
