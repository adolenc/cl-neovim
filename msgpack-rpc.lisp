(in-package #:cl-neovim)

(defparameter *msg-id* 0 "Unique id for messages.")
(defparameter *socket* NIL "Socket used for reading/writing.")

(eval-when (:compile-toplevel)
  (defun symbol-append (&rest symbols) 
    (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))
  (defun mklst (obj) (if (listp obj) obj (list obj)))
  (defun keep-lsts-with (kwd lst) (remove-if-not #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
  (defun remove-lsts-with (kwd lst) (remove-if #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
  (defun first-els (lst) (mapcar #'(lambda (c) (first (mklst c))) (mapcar #'mklst lst))))

(defmacro msg-rpc-type (type-name type-id &rest components)
  (let ((make-name (symbol-append 'make- type-name))
        (send-name (symbol-append 'send- type-name))
        (predicate-name (symbol-append type-name 'p))
        (parse-name (symbol-append 'parse- type-name)) 
        (components (mapcar #'mklst components)))
    `(progn
       (defun ,make-name ,(first-els (remove-lsts-with :make components))
         (concatenate '(vector (unsigned-byte 8)) (encode (list ,type-id ,@(mapcar #'(lambda (c) (or (getf (rest c) :make) (first c))) components)))))
       (defun ,send-name ,(first-els (remove-lsts-with :make components))
         (send (,make-name ,@(first-els (remove-lsts-with :make components)))))
       (defun ,predicate-name (msg)
         (= (first msg) ,type-id))
       (defun ,parse-name (msg)
         (destructuring-bind ,(cons 'msg-type (first-els components)) msg
           (declare (ignore msg-type))
           ,@(mapcar #'(lambda (c) (getf (rest c) :parse)) (keep-lsts-with :parse components))
           (values ,@(first-els (remove-lsts-with :dont-export components))))))))

(msg-rpc-type request 0
              (msg-id :make (incf *msg-id*))
              msg-method
              msg-params)
(msg-rpc-type response 1
              msg-id
              (msg-error :parse (if msg-error (error (second msg-error))) :dont-export T)
              msg-result)
(msg-rpc-type notification 2
              msg-method
              msg-params)

(defparameter *global-event-base* nil "Event base of the running event loop.")

(defun handle-new-msg (socket data)
  "Handle a new message based on its type and contents."
  (let* ((*decoder-prefers-lists* T)
         (*extended-types* *nvim-type-list*)
         (mpk::*bin-as-string* T)
         (msg (decode data)))
    (cond ((requestp msg) 
           (multiple-value-bind (msg-id msg-method msg-params) (parse-request msg)
             (format t "Received request [~A] ~A(~{~A~^, ~})~%" msg-id msg-method (first msg-params))
             (send-response msg-id (format NIL "Replying to request [~A] ~A(~{~A~^, ~})" msg-id msg-method (first msg-params)) NIL)))
          ((responsep msg) 
           (multiple-value-bind (msg-id msg-result) (parse-response msg)
             (format t "Received response [~A] ~A~%" msg-id msg-result)
             (force-output)))
          ((notificationp msg)
           (multiple-value-bind (msg-method msg-params) (parse-notification msg)
             (format t "Received notification ~A(~{~A~^, ~})~%" msg-method (first msg-params))
             (force-output))))))

(defun send (bytes)
  "Send bytes via *socket*."
  (if *socket*
    (if cl-async-base:*event-base*
      (as:write-socket-data *socket* bytes :force t)    
      ;; Because we can't just send message from a thread doesn't have event-loop
      ;; running and since there doesn't seem to be a better way of doing this
      ;; in cl-async, we have to make this thread know about an event loop
      ;; running in the other thread first.
      (let* ((cl-async-base:*event-base* *global-event-base*)
             (cl-async-base:*function-registry* (cl-async-base:event-base-function-registry cl-async-base:*event-base*)))
        (as:write-socket-data *socket* bytes :force t)))))

(defun run-listener (listener &rest listener-args)
  "Run event loop with specified listener and listener arguments."
  (as:with-event-loop ()
    (setf *global-event-base* cl-async-base:*event-base*
          *socket* (apply listener (append listener-args
                                           (list #'handle-new-msg))))
    (as:signal-handler 2 #'(lambda (sig)
                             (declare (ignore sig))
                             (as:exit-event-loop)))))

(defun run (&key (host "127.0.0.1") (port 7777) filename)
  "Run listener in an event loop inside a background thread."
  (bt:make-thread (lambda () (cond (filename        (run-listener #'as:pipe-connect filename))
                                   ((and host port) (run-listener #'as:tcp-connect host port))))
                  :name "Event loop"))

