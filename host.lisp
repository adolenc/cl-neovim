(load "~/.sbclrc")
(ql:quickload 'cl-neovim :silent t)

(nvim:defn "specs" :sync (path)
  (setf nvim::*specs* NIL)
  (with-output-to-string (*standard-output*)
    (load path))
  (make-array (length nvim::*specs*) :initial-contents nvim::*specs*))

(nvim:defn "poll" :sync (path)
  "ok")

(defun load-plugin (path)
  (with-output-to-string (*standard-output*)
    (with-open-file (s "/tmp/s.log" :direction :output :if-exists :append :if-does-not-exist :create)
      (format s "[~A]~%" path))
    (setf nvim::*path* path)
    (load path)))

(if (rest *posix-argv*)
  (mapcar #'load-plugin (rest *posix-argv*)))

(nvim:connect)
