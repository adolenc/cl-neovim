;;;; cl-msgpack-rpc.asd

(asdf:defsystem #:cl-msgpack-rpc
  :description "Implementation of client side for msgpack-rpc."
  :author "Andrej Dolenc andrej.dolenc@student.uni-lj.si"
  :depends-on (#:cl-messagepack #:cl-async)
  :serial t
  :components ((:file "package")
               (:file "cl-msgpack-rpc")))

