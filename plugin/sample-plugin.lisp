(load "~/.sbclrc")
(ql:quickload 'cl-neovim :silent t)


(defparameter *calls* 0 "Counter for calls.")

(nvim:defcmd cmd (range &rest args)
  (:range "" :nargs "*" :sync T)
  (increment-calls)
  (format t "Command: Called ~A times, args: ~A, range: ~A" *calls* args range))

(nvim:defautocmd buf-enter (filename)
  (:pattern "*.lisp" :eval "expand(\"<afile>\")" :sync T)
  (increment-calls)
  (format t "Autocmd: Called ~A times, file: ~A" *calls* filename)) 

(nvim:defun func (&rest args)
  (:pattern)
  (increment-calls)
  (format t "Function: Called ~A times, args: ~A" *calls* args))

(defun increment-calls ()
  (if (= *calls* 5)
    (error "Too many calls!")
    (incf *calls*)))


(nvim:connect)
