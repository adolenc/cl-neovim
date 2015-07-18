;;;; cl-neovim.lisp
(in-package #:cl-neovim)

(defparameter *id* 0)

(defun send-msg (socket msg)
  "Send encoded msg to the socket."
  (loop for m across msg
        do (write-byte m (socket-stream socket))
        finally (force-output (socket-stream socket))))

(defun parse-result (result)
  "Parse the result received from nvim socket."
  (decode result))

(defun receive-result (socket)
  "Decode result from the socket."
  (let ((buffer (make-array 0 :element-type 'unsigned-byte :adjustable t :fill-pointer t)))
    (wait-for-input socket)
    (loop with byte
          while (listen (socket-stream socket))
          do (setf byte (read-byte (socket-stream socket)))
          (when (= byte 0)
            (return t))
          (vector-push-extend byte buffer))
    (parse-result buffer)))

(defun command->msg (command &optional args)
  "Encode nvim command and optional args into msgpack packet."
  (encode `(0 ,(incf *id*) ,command ,args)))

(defun send-command (socket command &rest args)
  "Send nvim command to neovim socket and return the result."
  (let ((msg (command->msg command args)))
    (send-msg socket msg)
    (receive-result socket)))

; Hello world:
; (send-command (socket-connect #(127 0 0 1) 7777 :element-type '(unsigned-byte 8))
;               "vim_command"
;               "echo 'hello from common lisp!'")
