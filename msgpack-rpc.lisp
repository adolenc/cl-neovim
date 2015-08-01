(in-package #:cl-neovim)

(defparameter *msg-id* 0)
(defparameter *socket* NIL)
(defparameter *listener-thread* NIL)

(eval-when (:compile-toplevel)
  (defun symbol-append (&rest symbols) 
    (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))
  (defun mklst (obj) (if (listp obj) obj (list obj)))
  (defun keep-lsts-with (kwd lst) (remove-if-not #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
  (defun remove-lsts-with (kwd lst) (remove-if #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
  (defun first-els (lst) (mapcar #'(lambda (c) (first (mklst c))) (mapcar #'mklst lst))))

(defmacro msg-rpc-type (type-name type-id &rest components)
  (let ((make-name (symbol-append 'make- type-name))
        (predicate-name (symbol-append type-name 'p))
        (parse-name (symbol-append 'parse- type-name)) 
        (components (mapcar #'mklst components)))
    `(progn
       (defun ,make-name ,(first-els (remove-lsts-with :make components))
         (concatenate '(vector (unsigned-byte 8)) (encode (list ,type-id ,@(mapcar #'(lambda (c) (or (getf (rest c) :make) (first c))) components)))))
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
              (msg-error :parse (if msg-error (error (byte-array->string (second msg-error)))) :dont-export T)
              msg-result)
(msg-rpc-type notification 2
              msg-method
              msg-params)

(defun byte-array->string (arr)
  "Convert array of (unsigned-byte 8) to string."
  (octets-to-string (concatenate '(vector (unsigned-byte 8)) arr) :encoding :utf-8))

(defparameter *global-event-base* nil)

(defun run-listener (listener &rest listener-args)
  (as:with-event-loop ()
    (format t "Starting listener...~%")
    (setf *global-event-base* cl-async-base:*event-base*
          *socket* (apply listener (append listener-args
                                           (list #'handle-new-msg))))
    (as:signal-handler 2 #'(lambda (sig)
                             (declare (ignore sig))
                             (as:exit-event-loop)))))


(defun handle-new-msg (socket data)
  (let* ((*decoder-prefers-lists* T)
         (*extended-types* *nvim-type-list*)
         (mpk::*bin-as-string* T)
         (msg (decode data)))
    (cond ((requestp msg) 
           (multiple-value-bind (msg-id msg-method msg-params) (parse-request msg)
             (format t "Received request [~A] ~A(~{~A~^, ~})~%" msg-id msg-method (first msg-params))
             (send-msg (make-response msg-id (format NIL "Replying to request [~A] ~A(~{~A~^, ~})" msg-id msg-method (first msg-params)) NIL))))
          ((responsep msg) 
           (multiple-value-bind (msg-id msg-result) (parse-response msg)
             (format t "Received response [~A] ~A~%" msg-id msg-result)
             (force-output)))
          ((notificationp msg)
           (multiple-value-bind (msg-method msg-params) (parse-notification msg)
             (format t "Received notification ~A(~{~A~^, ~})~%" msg-method (first msg-params))
             (force-output))))))

(defun send-msg (msg)
  (if *socket*
    (if cl-async-base:*event-base*
      (as:write-socket-data *socket* msg :force t)    
      ;; Because we can't just send message from a thread doesn't have event-loop
      ;; running and since there doesn't seem to be a better way of doing this
      ;; in cl-async, we have to make this thread know about an event loop
      ;; running in the other thread first.
      (let* ((cl-async-base:*event-base* *global-event-base*)
             (cl-async-base:*data-registry* (cl-async-base:event-base-data-registry cl-async-base:*event-base*))
             (cl-async-base:*function-registry* (cl-async-base:event-base-function-registry cl-async-base:*event-base*))
             ; (*buffer-writes* *buffer-writes*)
             ; (*buffer-size* *buffer-size*)
             ; (*output-buffer* (static-vectors:make-static-vector *buffer-size* :element-type 'octet))
             ; (*input-buffer* (static-vectors:make-static-vector *buffer-size* :element-type 'octet))
             ; (*data-registry* (event-base-data-registry *event-base*))
             ; (*function-registry* (event-base-function-registry *event-base*))
             ; (callbacks nil)
             )
        (as:write-socket-data *socket* msg :force t)))))

(defun run (&key (host "127.0.0.1") (port 7777) fn)
  (bt:make-thread (lambda () (cond (fn              (run-listener #'as:pipe-connect fn))
                                   ((and host port) (run-listener #'as:tcp-connect host port))))
                  :name "Event loop")
  (format *standard-output* "Event loop is running..."))
