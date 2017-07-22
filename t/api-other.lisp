(in-package :cl-neovim-tests)
(in-suite api-other-test-suite)


(test other-call-atomic
  (with-fixture cleanup ()
    (setf (nvim:buffer-lines 0 0 -1 T) '("first"))
    (is (equal '("first" NIL 13 NIL)
               (call-atomic ()
                 (nvim:call/s t "nvim_get_current_line")
                 (nvim:call/a t "nvim_set_var" "avar" 13)
                 (nvim:var "avar")
                 (setf (nvim:current-line/a) "second"))))

    (is (equal '(NIL NIL T "string")
               (call-atomic ()
                 (setf (nvim:var "avar") T
                       (nvim:var "bvar") "string")
                 (nvim:var "avar")
                 (nvim:var "bvar"))))

    (is (equal '(NIL NIL NIL "tmp_avar" "tmp_bvar" "tmp_cvar")
               (call-atomic ()
                 (let ((vars '("avar" "bvar" "cvar")))
                   (dolist (var vars)
                     (setf (nvim:var var) (format nil "tmp_~A" var)))
                   (dolist (var vars)
                     (nvim:var var))))))

    (signals mrpc:rpc-error
             (call-atomic ()
               (setf (nvim:var "avar") 5)
               (nvim:buffer-lines 0 10 20 T)
               (setf (nvim:var "avar") 10)))

    (let ((b (nvim:current-buffer)))
      (is (equal '(NIL 5)
                 (handler-bind ((mrpc:rpc-error #'continue))
                   (call-atomic ()
                     (setf (nvim:var "avar") 5)
                     (nvim:var "avar")
                     (nvim:buffer-lines b 10 20 T)
                     (setf (nvim:var "avar") 10))))))))
