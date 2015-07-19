;;;; cl-neovim.asd

(asdf:defsystem #:cl-neovim
  :description "Common lisp client for neovim"
  :author "Andrej Dolenc andrej.dolenc@student.uni-lj.si"
  :depends-on (#:cl-messagepack #:usocket #:babel)
  :serial t
  :components ((:file "package")
               (:file "cl-neovim")))

