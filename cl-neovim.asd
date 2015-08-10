;;;; cl-neovim.asd

(asdf:defsystem #:cl-neovim
  :description "Common lisp client for neovim"
  :author "Andrej Dolenc andrej.dolenc@student.uni-lj.si"
  :depends-on (#:babel #:split-sequence #:cl-msgpack-rpc)
  :serial t
  :components ((:file "src/package")
               (:file "src/cl-neovim")
               (:file "src/interface")))

