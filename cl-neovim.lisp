;;;; cl-neovim.lisp
(in-package #:cl-neovim)

(defparameter *msg-id* 0)
(defparameter *nvim-type-list* (define-extension-types
                                 '(:numeric
                                    0
                                    Buffer
                                    Window
                                    Tabpage)))
(defparameter *socket* NIL)

(defun connect (&optional (host #(127 0 0 1)) (port 7777))
  "Connect to the nvim process (via TCP socket)."
  (setf *socket* (socket-connect host port :element-type '(unsigned-byte 8))))

(defun establish-listener (socket)
  "Basic polling loop for input from nvim."
  (loop do (progn (wait-for-input socket)
                  (multiple-value-bind (id req args) (get-result socket)
                    (format t "received: ~A ~A ~A~%" id req args)
                    (send-msg socket (command->response "OK" id))))))

(defun send-msg (socket msg)
  "Send encoded msg to nvim."
  (loop for m across msg
        do (write-byte m (socket-stream socket))
        finally (force-output (socket-stream socket))))

(defun byte-array->string (arr)
  "Convert array of (unsigned-byte 8) to string."
  (octets-to-string (concatenate '(vector (unsigned-byte 8)) arr) :encoding :utf-8))

(defun parse-request (request)
  "Parse the request received from nvim socket."
  (let ((msg-id (elt request 1))
        (req (elt request 2))
        (args (elt request 3)))
    (values msg-id (byte-array->string req) args)))

(defun parse-response (response)
  "Parse the response received from nvim socket."
  (let ((msg-id (elt response 1))
        (err (elt response 2))
        (result (elt response 3)))
    (if err
      (values NIL msg-id (byte-array->string (elt err 1)))
      (values T msg-id result)))) 

(defun msg-type (msg)
  (if (= (elt msg 0) 0) 'request 'response))

(defun get-result (socket)
  "Wait for response from nvim."
  (let* ((*extended-types* *nvim-type-list*)
         (msg (decode-stream (socket-stream socket)))
         (msg-parsing-fun (if (eq (msg-type msg) 'request) #'parse-request #'parse-response)))
    (funcall msg-parsing-fun msg)))

(defun command->request (command &optional args)
  "Encode nvim command and optional args into msgpack packet."
  (let ((*extended-types* *nvim-type-list*))
    (encode `(0 ,(incf *msg-id*) ,command ,(or args #())))))

(defun command->response (command msg-id &optional args)
  "Encode nvim command and optional args into msgpack packet."
  (let ((*extended-types* *nvim-type-list*))
    (encode `(1 ,msg-id ,command ,(or args #())))))

(defun send-command (socket command &rest args)
  "Send nvim command to neovim socket and return the result."
  (let ((msg (command->request command args)))
    (send-msg socket msg)
    (get-result socket)))
