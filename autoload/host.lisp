(defvar *here* #.(namestring (or *compile-file-pathname* *load-pathname* *default-pathname-defaults*)))

(with-open-stream (*standard-output* (make-broadcast-stream)) ; make quicklisp quiet about fetching packages
  (let ((*trace-output* *standard-output*)
        (*error-output* *standard-output*)
        (*debug-io* *standard-output*)) ; cffi is too chatty on *debug-io*
    (when *here*
      (let ((plugin-dir (subseq *current-dir* 0 (1+ (search "/autoload/" *here* :from-end T)))))
        (push plugin-dir asdf:*central-registry*))
      (ql:register-local-projects))
    (ql:quickload :cl-neovim :silent T)))

(defmacro def-nvim-mrpc-cb (name args &body body)
  `(mrpc::register-callback nvim::*nvim-instance* ,name
     #'(lambda ,args
         ,@body)))


(defparameter *loaded-plugin-specs* (list))

(nvim::connect-stdio)

(defun same-callback-p (callback1 callback2)
  (and (string= (gethash "type" callback1) (gethash "type" callback2))
       (string= (gethash "name" callback1) (gethash "name" callback2))
       (if (and (string= (gethash "type" callback1) "autocmd")
                (string= (gethash "type" callback2) "autocmd"))
         (string= (gethash "pattern" (gethash "opts" callback1))
                  (gethash "pattern" (gethash "opts" callback2)))
         T)))

(defun load-plugin (path)
  (let ((nvim::*specs* '())
        (nvim::*path* path))
    (handler-case (progn (load path)
                         (unless (assoc path *loaded-plugin-specs* :test #'equal)
                           (let ((spec (remove-duplicates nvim::*specs*  ; we need to remove all duplicate definitions except the last one
                                                          :from-end T
                                                          :test #'same-callback-p)))
                             (push (cons path spec) *loaded-plugin-specs*))))
      (error (desc)
         (format t "Failed to load plugin `~A':~%~A" path desc)
         (nvim:command (format nil "echo 'Failed to load lisp plugin ~A:~%~A'" path (substitute #\` #\' (format nil "~A" desc))))))))

(def-nvim-mrpc-cb "specs" (path)
  ; We always preload the plugins via LoadPlugins rpc call so specs are already available
  (rest (assoc path *loaded-plugin-specs* :test #'equal)))

(def-nvim-mrpc-cb "Poll" ()
  "ok")

(def-nvim-mrpc-cb "LoadPlugins" (plugins)
  (nvim::redirect-output (nvim::*log-stream*)
    (dolist (plugin-file-path plugins)
      (let ((plugin-path (subseq plugin-file-path 0 (search "rplugin/lisp/" plugin-file-path))))
        (push plugin-path asdf:*central-registry*)))
    (ql:register-local-projects)
    (map NIL #'load-plugin plugins)))

(setf nvim::*using-host* T)
(let ((log-file (uiop:getenv "NVIM_LISP_LOG_FILE"))
      (log-level (uiop:getenv "NVIM_LISP_LOG_LEVEL")))
  (when log-file
    (nvim:enable-logging :stream (open log-file :direction :output :if-does-not-exist :create :if-exists :append)
                         :level (if log-level (alexandria:make-keyword log-level) :info))
    (format nvim::*log-stream* "Loaded cl-neovim from ~A~%" (asdf:system-source-directory :cl-neovim))))
(nvim::redirect-output (nvim::*log-stream*)
  (handler-case (mrpc::run-forever (mrpc::event-loop nvim::*nvim-instance*))
    (error (desc)
      (format nvim::*log-stream* "Event loop received error:~%~A~%Closing lisp host.~%" desc) 
      (force-output nvim::*log-stream*)
      (close nvim::*log-stream*))))
