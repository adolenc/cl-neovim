(in-package #:cl-neovim)

(defparameter *msg-id* 0)
(defparameter *socket* NIL)

(defun connect (&optional (host #(127 0 0 1)) (port 7777))
  "Connect to the TCP socket."
  (setf *socket* (socket-connect host port :element-type '(unsigned-byte 8))))

(defun establish-listener ()
  "Basic polling loop for input from *socket*"
  (unless *socket* (connect))
  (let ((*decoder-prefers-lists* T)
        (*extended-types* *nvim-type-list*))
    (loop do (progn (wait-for-input *socket*)
                    (multiple-value-bind (msg-id msg-result) (get-result)
                      (send-msg (make-rpc-response msg-id NIL "Working!")))))))

(defun send-msg (msg)
  "Send encoded msg to *socket*"
  (loop for m across msg
        do (write-byte m (socket-stream *socket*))
        finally (force-output (socket-stream *socket*))))

(defun get-result ()
  "Wait for response from *socket*."
  (parse-msg (decode-stream (socket-stream *socket*))))

(defun parse-msg (msg)
  "Call appropriate parse function based on type of message."
  (funcall (cdr (assoc (first msg) *msg-parsers*)) msg))

(defun byte-array->string (arr)
  "Convert array of (unsigned-byte 8) to string."
  (octets-to-string (concatenate '(vector (unsigned-byte 8)) arr) :encoding :utf-8))

(eval-when (:compile-toplevel)
  (defun symbol-append (&rest symbols) 
    (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))
  (defun mklst (obj) (if (listp obj) obj (list obj)))
  (defun keep-lsts-with (kwd lst) (remove-if-not #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
  (defun remove-lsts-with (kwd lst) (remove-if #'(lambda (c) (getf (cdr c) kwd)) (mapcar #'mklst lst)))
  (defun first-els (lst) (mapcar #'(lambda (c) (first (mklst c))) (mapcar #'mklst lst)))
  (defparameter *msg-parsers* '()))

(defmacro msg-rpc-type (type-name type-id &rest components)
  (let ((make-name (symbol-append 'make-rpc- type-name))
        (predicate-name (symbol-append 'rpc- type-name '-p))
        (parse-name (symbol-append 'parse-rpc- type-name)) 
        (components (mapcar #'mklst components)))
    `(progn
       (defun ,make-name ,(first-els (remove-lsts-with :make components))
         (encode (list ,type-id ,@(mapcar #'(lambda (c) (or (getf (rest c) :make) (first c))) components))))
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
