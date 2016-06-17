(asdf:defsystem #:cl-neovim
  :description "Common Lisp client for Neovim"
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:babel
               #:split-sequence
               #:form-fiddle
               #:cl-messagepack-rpc)
  :serial T
  :components ((:module "src"
                :components ((:file "package")
                             (:file "utils")
                             (:file "vim-utils")
                             (:file "cl-neovim")
                             (:file "repl")
                             (:file "callbacks")
                             (:file "interface")
                             (:file "api"))))
  :in-order-to ((test-op (test-op cl-neovim-tests))))
