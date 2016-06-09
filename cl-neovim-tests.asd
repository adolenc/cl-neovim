(asdf:defsystem #:cl-neovim-tests
  :description "Tests for cl-neovim."
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:cl-neovim
               #:fiveam)
  :serial t
  :components ((:file "t/package")
               (:file "t/setup")
               (:file "t/common"))
  :perform (test-op (op c)
             (uiop:symbol-call '#:fiveam '#:run!
                               (uiop:intern* :neovim-test-suite :cl-neovim-tests))))
