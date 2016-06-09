#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :cl-neovim :silent t)

(defmacro def-nvim-mrpc-cb (name args &body body)
  `(mrpc::register-callback nvim::*nvim-instance* ,name
     #'(lambda ,args
         ,@body)))


(defparameter *loaded-plugin-specs* (list))

(nvim:connect)

(defun load-plugin (path)
  (let ((nvim::*specs* '())
        (nvim::*path* path))
    (load path)
    (unless (assoc path *loaded-plugin-specs* :test #'equal)
      (push (cons path nvim::*specs*) *loaded-plugin-specs*))))

(def-nvim-mrpc-cb "specs" (path)
  ; Either the plugin was already loaded in which case the specs should be available, or we need to load it
  (or (rest (assoc path *loaded-plugin-specs* :test #'equal))
      (let ((nvim::*specs* '()))
        (load-plugin path)
        nvim::*specs*)))

(def-nvim-mrpc-cb "Poll" ()
  "ok")

(def-nvim-mrpc-cb "EnableDebugging" (filename)
  (setf nvim:*debug-stream* (open filename :direction :output :if-does-not-exist :create :if-exists :append))
  T)

(def-nvim-mrpc-cb "LoadPlugins" (plugins)
  (let ((*standard-output* nvim:*debug-stream*)
        (*error-output* nvim:*debug-stream*))
    (map NIL #'load-plugin plugins)))

(setf nvim::*using-host* T
      nvim:*debug-stream* (make-broadcast-stream))
(mrpc::run-forever (mrpc::event-loop nvim::*nvim-instance*))
