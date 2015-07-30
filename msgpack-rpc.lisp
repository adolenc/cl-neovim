(in-package #:cl-neovim)

(defparameter *msg-id* 0)
(defparameter *socket* NIL)
(defparameter *listener-thread* NIL)

(eval-when (:compile-toplevel)
  (defparameter *msg-parsers* '()) 
  (defun symbol-append (&rest symbols) 
    (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))
  (defun mklst (obj) (if (listp obj) obj (list obj)))
  (defun keep-lsts-with (kwd lst) (remove-if-not #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
  (defun remove-lsts-with (kwd lst) (remove-if #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
  (defun first-els (lst) (mapcar #'(lambda (c) (first (mklst c))) (mapcar #'mklst lst))))

(defmacro msg-rpc-type (type-name type-id &rest components)
  (let ((make-name (symbol-append 'make-rpc- type-name))
        (predicate-name (symbol-append 'rpc- type-name '-p))
        (parse-name (symbol-append 'parse-rpc- type-name)) 
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
           (values ,@(first-els (remove-lsts-with :dont-export components)))))
       (setf *msg-parsers* (acons ,type-id #',parse-name *msg-parsers*)))))

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


(defun connect (&optional (host #(127 0 0 1)) (port 7777))
  "Connect to the TCP socket."
  (setf *socket* (socket-connect host port :element-type '(unsigned-byte 8))))

; (defun send-msg (msg)
;   "Send encoded msg to *socket*"
;   (loop for m across msg
;         do (write-byte m (socket-stream *socket*))
;         finally (force-output (socket-stream *socket*))))

(defun get-result ()
  "Wait for response from *socket*."
  (parse-msg (decode-stream (socket-stream *socket*))))

(defun parse-msg (msg)
  "Call appropriate parse function based on type of message."
  (funcall (cdr (assoc (first msg) *msg-parsers*)) msg))

(defun byte-array->string (arr)
  "Convert array of (unsigned-byte 8) to string."
  (octets-to-string (concatenate '(vector (unsigned-byte 8)) arr) :encoding :utf-8))


(defparameter *global-event-base* nil)
;
; (defun tcp-client (host port)
;   (format t "Starting TCP client.~%")
;   (setf *global-event-base* cl-async-util::*event-base*
;         *socket* (as:tcp-connect host port #'handle-new-msg
;                                  (lambda (err) (format t "listener event: ~a~%" err))))
;   (as:signal-handler 2 (lambda (sig)
;                          (declare (ignore sig))
;                          (as:exit-event-loop))))
; (defun pipe-client (name)
;   (format t "Starting pipe client.~%")
;   (setf *global-event-base* cl-async-util::*event-base*
;         *socket* (as:pipe-connect name #'handle-new-msg))
;   (as:signal-handler 2 (lambda (sig)
;                          (declare (ignore sig))
;                          (as:exit-event-loop))))
;
; (defun handle-new-msg (socket data)
;   (format t "RECEIVED ~A ~A~%" data (decode data))
;   (let* ((*decoder-prefers-lists* T)
;          (*extended-types* *nvim-type-list*)
;          (mpk::*bin-as-string* T)
;          (msg (decode data)))
;     (cond ((rpc-request-p msg) 
;            (multiple-value-bind (msg-id msg-method msg-params) (parse-msg msg)
;              (as:write-socket-data socket
;                                    (make-rpc-response msg-id (format NIL "Replying to request ~A ~A(~{~A~^, ~})" msg-id msg-method (first msg-params)) NIL))))
;           )))
;
; (defun send-msg (msg)
;   (if *socket*
;     (let ((cl-async-util::*event-base* *global-event-base*))
;       (as:write-socket-data *socket* msg))  
;     ; (as:write-socket-data *socket* msg)  
;     ; (write-sequence msg *socket*)
;     ))


(defun tcp-client (host port)
  (format t "Starting TCP client.~%")
  (setf *global-event-base* cl-async-base:*event-base*
        *socket* (as:tcp-connect host port #'handle-new-msg
                                 (lambda (err) (format t "listener event: ~a~%" err))))
  (as:signal-handler 2 (lambda (sig)
                         (declare (ignore sig))
                         (as:exit-event-loop))))
(defun pipe-client (name)
  (format t "Starting pipe client.~%")
  (setf *global-event-base* cl-async-util::*event-base*
        *socket* (as:pipe-connect name #'handle-new-msg))
  (as:signal-handler 2 (lambda (sig)
                         (declare (ignore sig))
                         (as:exit-event-loop))))

(defun handle-new-msg (socket data)
  (format t "RECEIVED ~A~%" (decode data))
  (force-output)
  (let* ((*decoder-prefers-lists* T)
         (*extended-types* *nvim-type-list*)
         (mpk::*bin-as-string* T)
         (msg (decode data)))
    (format t "INSIDE LET: ~A~%" msg)
    (cond ((rpc-request-p msg) 
           (multiple-value-bind (msg-id msg-method msg-params) (parse-msg msg)
             (as:write-socket-data socket
                                   (make-rpc-response msg-id (format NIL "Replying to request ~A ~A(~{~A~^, ~})" msg-id msg-method (first msg-params)) NIL) :force t)
             (force-output)))
      ((rpc-response-p msg) 
           (multiple-value-bind (msg-id msg-result) (parse-msg msg)
             (format NIL "Received response ~A ~A~%" msg-id msg-result)
             (force-output))))))

(defun send-msg (msg)
  (if *socket*
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
      (format t "TRYING TO SEND MSG: ~A~%" msg)
      (as:write-socket-data *socket* msg :force t))))

(defun establish-listener (&key (host "127.0.0.1") (port 7777) fn)
  ; (cond (fn (as:start-event-loop #'(lambda () (pipe-client fn))))
  ;        ((and host port) (as:start-event-loop #'(lambda () (tcp-client host port)))))

  (setf *listener-thread* (bt:make-thread (lambda () (cond (fn (as:start-event-loop #'(lambda () (pipe-client fn))))
                                                           ((and host port) (as:start-event-loop #'(lambda () (tcp-client host port)))))) :name "Event loop"))
  ; (format *standard-output* "WAITING...")
)

(defun kill-listener ()
  (bt:join-thread *listener-thread*))
