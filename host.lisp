(load "~/.sbclrc")
(ql:quickload 'cl-neovim :silent t)


(nvim:defn "specs" :sync (path)
  (setf nvim::*specs* NIL)
  (with-output-to-string (*standard-output*)
    (load path))
  (make-array (length nvim::*specs*) :initial-contents nvim::*specs*))

(nvim:defn "poll" :sync ()
  "ok")

(nvim:connect)
