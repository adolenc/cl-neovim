(asdf:defsystem #:cl-neovim
  :description "Common lisp client for neovim"
  :author "Andrej Dolenc andrej.dolenc@student.uni-lj.si"
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
