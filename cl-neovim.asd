(asdf:defsystem #:cl-neovim
  :description "Common Lisp client for Neovim"
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:babel
               #:split-sequence
               #:form-fiddle
               #:cl-messagepack-rpc
               #:vom)
  :serial T
  :components ((:module "src"
                :serial T
                :components ((:file "package")
                             (:file "utils")
                             (:file "vim-utils")
                             (:file "cl-neovim")
                             (:file "logging")
                             (:file "repl")
                             (:file "callbacks")
                             (:file "interface")
                             (:file "api")
                             (:file "generated-api"))))
  :in-order-to ((test-op (test-op cl-neovim-tests))))
