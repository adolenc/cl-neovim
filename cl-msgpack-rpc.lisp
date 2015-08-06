(in-package #:cl-msgpack-rpc)

(defparameter *msg-id* 0 "Unique id for messages.")
(defparameter *socket* NIL "Socket used for reading/writing.")

(defparameter *active-requests* (make-hash-table) "Hash table holding active requests (and their eventual results).")
(defparameter *request-callbacks* (make-hash-table :test 'equal) "Hash table holding callbacks for requests.")
(defparameter *notification-callbacks* (make-hash-table :test 'equal) "Hash table holding callbacks for notifications.")

(defparameter *global-event-base* nil "Event base of the running event loop.")


(eval-when (:compile-toplevel)

(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))
(defun mklst (obj) (if (listp obj) obj (list obj)))
(defun keep-lsts-with (kwd lst) (remove-if-not #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
(defun remove-lsts-with (kwd lst) (remove-if #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
(defun first-els (lst) (mapcar #'(lambda (c) (first (mklst c))) (mapcar #'mklst lst)))

)

(defmacro msg-rpc-type (type-name type-id &rest components)
  (let ((make-name (symbol-append 'make- type-name))
        (send-name (symbol-append 'send- type-name))
        (predicate-name (symbol-append type-name 'p))
        (with-name (symbol-append 'with- type-name)) 
        (components (mapcar #'mklst components)))
    `(progn
       (defun ,make-name ,(first-els (remove-lsts-with :make components))
         (concatenate '(vector (unsigned-byte 8)) (mpk:encode (list ,type-id ,@(mapcar #'(lambda (c) (or (getf (rest c) :make) (first c))) components)))))
       (defun ,send-name ,(first-els (remove-lsts-with :make components))
         (send (,make-name ,@(first-els (remove-lsts-with :make components)))))
       (defun ,predicate-name (msg)
         (= (first msg) ,type-id))
       (defmacro ,with-name (msg &body body)
         `(destructuring-bind ,(cons 'msg-type ',(first-els components)) msg
             (declare (ignore msg-type))
             ,@body)))))

(msg-rpc-type request 0 (msg-id :make (incf *msg-id*)) msg-method msg-params)
(msg-rpc-type response 1 msg-id msg-error msg-result)
(msg-rpc-type notification 2 msg-method msg-params)

(defun handle-new-msg (socket data)
  "Handle a new message based on its type and contents."
  (let* ((mpk:*decoder-prefers-lists* T)
         (mpk::*bin-as-string* T)
         (msg (mpk:decode data)))
    (cond ((requestp msg)
           (with-request msg
             (handler-case (send-response msg-id NIL
                                          (apply (gethash msg-method *request-callbacks*)
                                                            (first msg-params)))
               (error (desc) (send-response msg-id (format nil "~A" desc) NIL)))))
          ((responsep msg)
           (with-response msg
             (fulfill (gethash msg-id *active-requests*) msg-result)
             (remhash msg-id *active-requests*)))
          ((notificationp msg)
           (with-notification msg
             (handler-case (apply (gethash msg-method *notifications-callbacks*) (first msg-params))
               (error (desc) (warn (format nil "Unhandled notification ~A(~{~A~^, ~}):~%~A~%" msg-method (first msg-params) desc)))))))))

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
             (cl-async-base:*data-registry* (cl-async-base:event-base-data-registry cl-async-base:*event-base*))
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

(defun make-promise () "A constructor for a quick and dirty promise." (cons NIL NIL))

(defun finish (promise)
  "Block the thread until a promise is resolved."
  (loop until (car promise) finally (return (cdr promise))))

(defun fulfill (promise result)
  "Fill the result into promise and toggle its resolved switch."
  (setf (cdr promise) result
        (car promise) T))

(defun request (fn &optional params (async NIL))
  "Send a request for function fn with params. Optionally specify you want to
   receive results asynchroniously, which returns a promise."
  (let ((result (make-promise)))
    (setf (gethash (1+ *msg-id*) *active-requests*) result)
    (send-request fn (or params #()))
    (if async
      result
      (finish result))))

(defun notify (fn &optional params)
  "Send a notification for function fn with params."
  (send-notification fn (or params #())))

(defun register-request-callback (name fn)
  "Register a function which will get called when server sends
   request for `name'."
  (setf (gethash name *request-callbacks*) fn))

(defun register-notification-callback (name fn)
  "Register a function which will get called when server sends
   request for `name'."
  (setf (gethash name *notification-callbacks*) fn))

(defun remove-request-callback (name)
  "Remove a registered request callback."
  (remhash name *request-callbacks*))

(defun remove-notification-callback (name)
  "Remove a registered notification callback."
  (remhash name *notification-callbacks*))

(defun run (&key host port filename)
  "Run listener in an event loop inside a background thread."
  (bt:make-thread (lambda () (cond (filename        (run-listener #'as:pipe-connect filename))
                                   ((and host port) (run-listener #'as:tcp-connect host port))
                                   (t (error "Currently only tcp/unix sockets are supported."))))
                  :name "Event loop"
                  :initial-bindings `((mpk:*extended-types* . ',*extended-types*))))
