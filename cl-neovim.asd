;;;; cl-neovim.asd

(asdf:defsystem #:cl-neovim
  :description "Common lisp client for neovim"
  :author "Andrej Dolenc andrej.dolenc@student.uni-lj.si"
  :depends-on (#:cl-messagepack #:usocket #:trivial-utf-8)
  :serial t
  :components ((:file "package")
               (:file "cl-neovim")))

