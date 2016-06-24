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
    (handler-case (progn (load path)
                         (unless (assoc path *loaded-plugin-specs* :test #'equal)
                           (let ((spec (remove-duplicates nvim::*specs*  ; we need to remove all duplicate definitions except the last one
                                                          :from-end T
                                                          :test #'(lambda (def-1 def-2)
                                                                    (if (string= (gethash "type" def-1) (gethash "type" def-2))
                                                                      (string= (gethash "name" def-1) (gethash "name" def-2)))))))
                             (push (cons path spec) *loaded-plugin-specs*))))
      (error (desc)
         (format t "Failed to load plugin `~A':~%~A" path desc)
         (nvim:command (format nil "echom 'Failed to load lisp plugin ~A:~%~A'" path desc))))))

(def-nvim-mrpc-cb "specs" (path)
  ; Either the plugin was already loaded in which case the specs should be available, or we need to load it
  (or (rest (assoc path *loaded-plugin-specs* :test #'equal))
      (let ((nvim::*specs* '()))
        (load-plugin path)
        nvim::*specs*)))

(def-nvim-mrpc-cb "Poll" ()
  "ok")

(def-nvim-mrpc-cb "EnableLogging" (filename)
  (setf nvim::*log-stream* (open filename :direction :output :if-does-not-exist :create :if-exists :append))
  T)

(def-nvim-mrpc-cb "LoadPlugins" (plugins)
  (nvim::redirect-output (nvim::*log-stream*)
    (map NIL #'load-plugin plugins)))

(setf nvim::*using-host* T
      nvim::*log-stream* (make-broadcast-stream))
(nvim::redirect-output (nvim::*log-stream*)
  (handler-case (mrpc::run-forever (mrpc::event-loop nvim::*nvim-instance*))
    (error (desc)
      (format nvim::*log-stream* "Event loop received error:~%~A~%Closing lisp host.~%" desc) 
      (force-output nvim::*log-stream*)
      (close nvim::*log-stream*))))
