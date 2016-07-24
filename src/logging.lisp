(in-package #:cl-neovim)


(defparameter *log-stream* *standard-output*)


(cl:defun enable-logging (&key (stream *standard-output*) (level :info))
  (setf *log-stream* stream
        vom:*log-stream* stream
        *print-pretty* NIL)
  (vom:config t level))

(cl:defmethod mrpc::send :before (event-loop socket bytes)
  (declare (ignore event-loop socket))
  (vom:debug1 "sending bytes:            ~S" bytes))

(cl:defmethod mrpc::callback-handler :before (session bytes)
  (declare (ignore session))
  (vom:debug1 "received bytes:           #~S" bytes))


(cl:defmethod mrpc::send-request :before (session id method params)
  (declare (ignore session))
  (vom:debug  "(cl-neovim -> nvim) [req] (~S ~S ~S)" id method params))

(cl:defmethod mrpc::send-notification :before (session method params)
  (declare (ignore session))
  (vom:debug "(cl-neovim -> nvim) [ntf] (~S ~S)" method params))

(cl:defmethod mrpc::send-response :before (session id error result)
  (declare (ignore session))
  (vom:debug "(cl-neovim -> nvim) [rsp] (~S ~S ~S)" id error result))

(cl:defmethod mrpc::on-message :before (session data)
  (declare (ignore session))
  (vom:debug "(nvim -> cl-neovim) [~A] ~S"
          (cond ((mrpc::requestp data) "req")
                ((mrpc::notificationp data) "ntf")
                ((mrpc::responsep data) "rsp"))
          data))


(cl:defmethod mrpc:register-callback :before (session method callback)
  (declare (ignore session callback))
  (vom:debug "Registering callback `~S'" method))

(cl:defmethod mrpc:remove-callback :before (session method)
  (declare (ignore session))
  (vom:debug "Removing callback `~S'" method))
