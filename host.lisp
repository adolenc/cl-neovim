(load "~/.sbclrc")
(ql:quickload 'cl-neovim :silent t)

(nvim:defunc "specs" :sync (path)
  (setf nvim::*specs* NIL)
  (with-output-to-string (*standard-output*)
    (load path))
  (make-array (length nvim::*specs*) :initial-contents nvim::*specs*))

(nvim:defunc "poll" :sync ()
  "ok")

(defun load-plugin (path)
  (with-output-to-string (*standard-output*)
    (setf nvim::*path* path)
    (load path)))

(if (rest *posix-argv*)
  (mapcar #'load-plugin (rest *posix-argv*)))

(nvim:connect)
