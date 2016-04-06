(in-package #:msgpack-rpc)

(defvar *msg-id* 0 "Unique id for messages.")
(defvar *socket* NIL "Socket used for reading/writing.")
(defvar *connection-type* NIL "Type of connection.")

(defvar *request-callbacks* (make-hash-table :test 'equal) "Hash table holding callbacks for requests.")
(defvar *notification-callbacks* (make-hash-table :test 'equal) "Hash table holding callbacks for notifications.")
(defvar *active-requests* (make-hash-table) "Hash table holding active requests (and their eventual results).")
(defvar *active-requests-lock* (bt:make-lock))

(defvar *global-event-base* nil "Event base of the running event loop.")


(eval-when (:compile-toplevel :load-toplevel)
  (defun symbol-append (&rest symbols) 
    (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))
  (defun mklst (obj)
    (if (listp obj) obj (list obj)))
  (defun keep-lsts-with (kwd lst)
    (remove-if-not #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
  (defun remove-lsts-with (kwd lst)
    (remove-if #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
  (defun first-els (lst)
    (mapcar #'(lambda (c) (first (mklst c))) (mapcar #'mklst lst))))

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
  (declare (ignore socket))
  "Handle a new message based on its type and contents."
  (let* ((mpk:*decoder-prefers-lists* T)
         (mpk:*decode-bin-as-string* T)
         (msg (mpk:decode data)))
    (format nvim:*debug-stream* "< ~A~%" msg)
    (force-output nvim:*debug-stream*)
    (cond ((requestp msg)
           (with-request msg
             (bt:make-thread
               #'(lambda () (handler-case (send-response msg-id NIL
                                                         (apply (gethash msg-method *request-callbacks*) msg-params))
               (error (desc) (send-response msg-id (format nil "~A" desc) NIL)))))))
          ((responsep msg)
           (with-response msg
             (bt:with-lock-held (*active-requests-lock*)
               (fulfill (gethash msg-id *active-requests*) msg-result)
               (remhash msg-id *active-requests*))))
          ((notificationp msg)
           (with-notification msg
             (bt:make-thread #'(lambda ()
               (handler-case (apply (gethash msg-method *notification-callbacks*) msg-params)
                 (error (desc) (warn (format nil "Unhandled notification ~A(~{~A~^, ~}):~%~A~%" msg-method msg-params desc)))))))))))

(defun send (bytes)
  "Send bytes via *socket*."
  (format nvim:*debug-stream* "> ~A~%" (mpk:decode bytes))
  (force-output nvim:*debug-stream*)
  (if (eq *connection-type* :stdio)
    (loop for b across bytes do (write-byte b *socket*) finally (force-output))
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
          (as:write-socket-data *socket* bytes :force t))))))

(defun run-listener (listener &rest listener-args)
  "Run event loop with specified listener and listener arguments."
  (as:with-event-loop ()
    (setf *global-event-base* cl-async-base:*event-base*
          *socket* (apply listener (append listener-args
                                           (list #'handle-new-msg))))
    (as:signal-handler 2 #'(lambda (sig)
                             (declare (ignore sig))
                             (as:exit-event-loop)))))

(defun collect-input ()
  "Block thread until data is available on *standard-input* and retrieve it."
  (loop until (listen)
        do (sleep 0.001)
        finally (return (loop while (listen)
                              collect (read-byte *standard-input*)))))

(defun run-io-listener ()
  "Run event loop listening to stdin and responding to stdout."
  (as:with-event-loop ()
    (setf *global-event-base* cl-async-base:*event-base*
          *socket* *standard-output*)
    (let* ((result nil)
           (notifier (as:make-notifier (lambda ()
                                         (handle-new-msg NIL result))
                                       :single-shot NIL)))
      (bt:make-thread (lambda ()
                        (loop do (progn (setf result (collect-input))
                                        (as:trigger-notifier notifier))))))
                      (as:signal-handler 2 #'(lambda (sig)
                                               (declare (ignore sig))
                                               (as:exit-event-loop))))) 


(defun make-promise () "A constructor for a quick and dirty promise." (cons NIL NIL))

(defun finish (promise)
  "Block the thread until a promise is resolved."
  (loop until (car promise) do (sleep 0.01) finally (return (cdr promise))))

(defun fulfill (promise result)
  "Fill the result into promise and toggle its resolved switch."
  (setf (cdr promise) result
        (car promise) T))

(defun request (fn &optional params (async NIL))
  "Send a request for function fn with params. Optionally specify you want to
   receive results asynchroniously, which returns a promise."
  (let ((result (make-promise)))
    (bt:with-lock-held (*active-requests-lock*)
      (setf (gethash (1+ *msg-id*) *active-requests*) result))
    (send-request fn (or params #()))
    (if async
      result
      (finish result))))

(defun notify (fn &optional params)
  "Send a notification for function fn with params."
  (send-notification fn (or params #()))
  NIL)

(defun register-request-callback (name fn)
  "Register a function which will get called when server sends
   request for `name'."
  (format nvim:*debug-stream* "REG-R: ~A~%" name)
  (force-output nvim:*debug-stream*)
  (setf (gethash name *request-callbacks*) fn))

(defun register-notification-callback (name fn)
  "Register a function which will get called when server sends
   request for `name'."
  (format nvim:*debug-stream* "REG-N: ~A~%" name)
  (force-output nvim:*debug-stream*)
  (setf (gethash name *notification-callbacks*) fn))

(defun remove-request-callback (name)
  "Remove a registered request callback."
  (remhash name *request-callbacks*))

(defun remove-notification-callback (name)
  "Remove a registered notification callback."
  (remhash name *notification-callbacks*))


(defun connect (&key host port file)
  "Run listener in an event loop inside a background thread."
  (if (or host port file)
    (bt:make-thread (lambda () (cond (file            (setf *connection-type* :pipe)
                                                      (run-listener #'as:pipe-connect file))
                                     ((and host port) (setf *connection-type* :tcp)
                                                      (run-listener #'as:tcp-connect host port))
                                     (t (error "You must specify both host and port."))))
                    :name "Event loop"
                    :initial-bindings `((mpk:*extended-types* . ',*extended-types*)))
    (with-open-file (*error-output* "/tmp/err.log" :direction :output :if-exists :append :if-does-not-exist :create)
      (setf *connection-type* :stdio)
      (run-io-listener))))
