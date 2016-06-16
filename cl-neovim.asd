(asdf:defsystem #:cl-neovim
  :description "Common Lisp client for Neovim"
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:babel
               #:split-sequence
               #:form-fiddle
               #:jonathan
               #:cl-messagepack-rpc)
  :serial t
  :components ((:file "src/package")
               (:file "src/utils")
               (:file "src/cl-neovim")
               (:file "src/repl")
               (:file "src/vim-utils")
               (:file "src/callbacks")
               (:file "src/interface")
               (:file "src/api")
               )
  :in-order-to ((test-op (test-op cl-neovim-tests))))
