;;;; cl-neovim.lisp
(in-package #:cl-neovim)

(defparameter *id* 0)

(defun send-msg (socket msg)
  "Send encoded msg to nvim."
  (loop for m across msg
        do (write-byte m (socket-stream socket))
        finally (force-output (socket-stream socket))))

(defun parse-response (response)
  "Parse the response received from nvim socket."
    (let* ((response (decode response))
           (msg-id (elt response 1))
           (err (elt response 2))
           (err-str (if err (octets-to-string (concatenate '(vector (unsigned-byte 8)) (elt err 1)) :encoding :utf-8)))
           (result (elt response 3)))
      (if err
        (values NIL msg-id err-str)
        (values T msg-id result)))) 

(defun get-result (socket)
  "Wait for response from nvim."
  (let ((buffer (make-array 0 :element-type 'unsigned-byte :adjustable t :fill-pointer t)))
    (wait-for-input socket)
    (loop with byte
          while (listen (socket-stream socket))
          do (setf byte (read-byte (socket-stream socket)))
          (when (= byte 13)
            (return t))
          (vector-push-extend byte buffer))
    (parse-response buffer)))

(defun command->msg (command &optional args)
  "Encode nvim command and optional args into msgpack packet."
  (encode `(0 ,(incf *id*) ,command ,(or args #()))))

(defun send-command (socket command &rest args)
  "Send nvim command to neovim socket and return the result."
  (let ((msg (command->msg command args)))
    (send-msg socket msg)
    (get-result socket)))

; Hello world:
(send-command (socket-connect #(127 0 0 1) 7777 :element-type '(unsigned-byte 8))
              "vim_del_current_line")
