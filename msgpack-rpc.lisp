(in-package #:cl-neovim)

(defparameter *msg-id* 0)
(defparameter *nvim-type-list* (define-extension-types
                                 '(0
                                   Buffer
                                   Window
                                   Tabpage)))
(defparameter *socket* NIL)

(defun connect (&optional (host #(127 0 0 1)) (port 7777))
  "Connect to the nvim process (via TCP socket)."
  (setf *socket* (socket-connect host port :element-type '(unsigned-byte 8))))

(defun establish-listener ()
  "Basic polling loop for input from nvim."
  (unless *socket* (connect))
  (loop do (progn (wait-for-input *socket*)
                  (multiple-value-bind (id req args) (get-result *socket*)
                    (format t "received: ~A ~A ~A~%" id req args)
                    (send-msg (command->response "OK" id))))))

(defun send-msg (msg)
  "Send encoded msg to nvim."
  (loop for m across msg
        do (write-byte m (socket-stream *socket*))
        finally (force-output (socket-stream *socket*))))

(defun byte-array->string (arr)
  "Convert array of (unsigned-byte 8) to string."
  (octets-to-string (concatenate '(vector (unsigned-byte 8)) arr) :encoding :utf-8))

(defun parse-request (request)
  "Parse the request received from nvim socket."
  (destructuring-bind (_ msg-id req args) request
    (declare (ignore _))
    (values msg-id (byte-array->string req) args)))

(defun parse-response (response)
  "Parse the response received from nvim socket."
  (destructuring-bind (_ msg-id err result) response
    (declare (ignore _))
    (if err
      (values NIL msg-id (byte-array->string (first err)))
      (values T msg-id result)))) 

(defun msg-type (msg)
  (if (= (first msg) 0) 'request 'response))

(defun get-result ()
  "Wait for response from nvim."
  (let* ((*decoder-prefers-lists* T)
         (*extended-types* *nvim-type-list*)
         (msg (decode-stream (socket-stream *socket*)))
         (msg-parsing-fun (if (eq (msg-type msg) 'request) #'parse-request #'parse-response)))
    (multiple-value-bind (succ msg-id result) (funcall msg-parsing-fun msg)
      (declare (ignore msg-id))
      (if succ result (error result)))))

(defun command->request (command &optional args)
  "Encode nvim command and optional args into msgpack packet."
  (let ((*extended-types* *nvim-type-list*)
         (*decoder-prefers-lists* T))
    (encode `(0 ,(incf *msg-id*) ,command ,(or args #())))))

(defun command->response (command msg-id &optional args)
  "Encode nvim command and optional args into msgpack packet."
  (let ((*extended-types* *nvim-type-list*)
        (*decoder-prefers-lists* T))
    (encode `(1 ,msg-id ,command ,(or args #())))))
