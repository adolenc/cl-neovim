#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :cl-neovim :silent t)

(defparameter *loaded-plugin-specs* (list))

(nvim:connect)

(defun load-plugin (path)
  (let ((nvim::*specs* '())
        (nvim::*path* path))
    (load path)
    (unless (assoc path *loaded-plugin-specs* :test #'equal)
      (push (cons path nvim::*specs*) *loaded-plugin-specs*))))

(nvim:defun "specs" :sync (path)
  ; Either the plugin was already loaded in which case the specs should be available, or we need to load it
  (or (rest (assoc path *loaded-plugin-specs* :test #'equal))
      (let ((nvim::*specs* '()))
        (load-plugin path)
        nvim::*specs*)))

(nvim:defun poll :sync ()
  "ok")

(nvim:defun enable-debugging :sync (filename)
  (setf nvim:*debug-stream* (open filename :direction :output :if-does-not-exist :create :if-exists :append))
  (format nvim:*debug-stream* "Debug enabled~%") (force-output nvim:*debug-stream*)
  T)

(nvim:defun load-plugins :sync (plugins)
  (let ((*standard-output* nvim:*debug-stream*)
        (*error-output* nvim:*debug-stream*))
    (map NIL #'load-plugin plugins)))

(setf nvim::*using-host* T)
(mrpc::run-forever (mrpc::event-loop nvim::*nvim-instance*))
