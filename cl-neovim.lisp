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

(defun send-msg (socket msg)
  "Send encoded msg to nvim."
  (loop for m across msg
        do (write-byte m (socket-stream socket))
        finally (force-output (socket-stream socket))))

(defun byte-array->string (arr)
  "Convert array of (unsigned-byte 8) to string."
  (octets-to-string (concatenate '(vector (unsigned-byte 8)) arr) :encoding :utf-8))

(defun parse-response (response)
  "Parse the response received from nvim socket."
    (let* ((msg-id (elt response 1))
           (err (elt response 2))
           (err-str (if err (byte-array->string (elt err 1))))
           (result (elt response 3)))
      (if err
        (values NIL msg-id err-str)
        (values T msg-id result)))) 

(defun get-result (socket)
  "Wait for response from nvim."
  (let ((*extended-types* *nvim-type-list*))
    (wait-for-input socket)
    (parse-response (decode-stream (socket-stream socket)))))

(defun command->msg (command &optional args)
  "Encode nvim command and optional args into msgpack packet."
  (let ((*extended-types* *nvim-type-list*))
    (encode `(0 ,(incf *msg-id*) ,command ,(or args #())))))

(defun send-command (socket command &rest args)
  "Send nvim command to neovim socket and return the result."
  (let ((msg (command->msg command args)))
    (send-msg socket msg)
    (get-result socket)))
