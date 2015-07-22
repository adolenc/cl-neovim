(in-package #:cl-neovim)

(defun send-command (command &rest args)
  "Send nvim command to neovim socket and return the result."
  (unless *socket* (connect))
  (let ((msg (command->request command args)))
    (send-msg *socket* msg)
    (get-result *socket*)))
