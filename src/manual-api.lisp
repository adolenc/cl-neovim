(in-package #:cl-neovim)


(cl:defun buffer-number (buffer &optional (instance *nvim-instance*))
  (declare (ignore instance))
  (mpk::decode (mpk::extension-type-id buffer)))

(cl:defun buffer-number/a (buffer &optional (instance *nvim-instance*))
  (declare (ignore instance))
  (mpk::decode (mpk::extension-type-id buffer)))

(cl:defun subscribe (event function &optional (instance *nvim-instance*))
  (mrpc:register-callback instance event function)
  (nvim:call/s instance "vim_subscribe" event))

(cl:defun subscribe/a (event function &optional (instance *nvim-instance*))
  (mrpc:register-callback instance event function)
  (nvim:call/a instance "vim_subscribe" event))

(cl:defun unsubscribe (event &optional (instance *nvim-instance*))
  (nvim:call/s instance "vim_unsubscribe" event)
  (mrpc:remove-callback instance event))

(cl:defun unsubscribe/a (event &optional (instance *nvim-instance*))
  (nvim:call/a instance "vim_unsubscribe" event)
  (mrpc:remove-callback instance event))

(defmacro call-atomic ((&optional (instance *nvim-instance*)) &rest body)
  `(destructuring-bind (results err)
                       (nvim:call/s ,instance "nvim_call_atomic"
                                    (let ((*should-capture-calls* T)
                                          (*captured-calls* (list)))
                                      ,@body
                                      (reverse *captured-calls*)))

     (restart-case
         (when err
           (error 'mrpc:rpc-error :message err))
       (continue () results))
     results))
