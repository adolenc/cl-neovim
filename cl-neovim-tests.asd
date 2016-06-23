(asdf:defsystem #:cl-neovim-tests
  :description "Tests for cl-neovim."
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:cl-neovim
               #:fiveam)
  :serial T
  :components ((:module "t"
                :components ((:file "package")
                             (:file "setup")
                             (:file "callbacks"))))
  :perform (test-op (op c)
             (uiop:symbol-call '#:fiveam '#:run!
                               (uiop:find-symbol* '#:neovim-test-suite :cl-neovim-tests))))
