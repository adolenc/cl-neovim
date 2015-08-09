(ql:quickload 'cl-neovim)


(defparameter *calls* 0 "Counter for calls.")

(defun increment-calls ()
  (if (= *calls* 5)
    (error "Too many calls!")
    (incf *calls*)))

(nvim:defcmd cmd :sync (&rest args)
  (opts (:range "" :nargs "*"))
  (increment-calls)
  (setf (nvim:current-line)
        (format nil "Command: Called ~A times, args: ~A, range: ~A" *calls* args 2)))

(nvim:defautocmd buf-enter :sync (filename)
  (opts (:pattern "*.lisp" :eval "expand(\"<afile>\")")) 
  (increment-calls)
  (setf (nvim:current-line)
        (format nil "Autocmd: Called ~A times, file: ~A" *calls* filename))) 

(nvim:defn func (&rest args)
  (increment-calls)
  (setf (nvim:current-line)
        (format nil "Function: Called ~A times, args: ~A" *calls* args)))
