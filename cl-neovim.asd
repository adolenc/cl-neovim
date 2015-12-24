(asdf:defsystem #:cl-neovim
  :description "Common Lisp client for Neovim"
  :author "Andrej Dolenc <andrej.dolenc@student.uni-lj.si>"
  :license "MIT"
  :depends-on (#:babel
               #:split-sequence
               #:form-fiddle
               #:cl-messagepack
               #:cl-async
               #:bordeaux-threads)
  :serial t
  :components ((:file "src/package")
               (:file "src/msgpack-rpc")
               (:file "src/cl-neovim")
               (:file "src/interface")))
