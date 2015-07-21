(in-package #:nvim)

(defun function-description (f)
  (labels ((decode-description (description)
             (cond ((and (arrayp description) (= (length description) 0)) '())
                   ((and (arrayp description) (numberp (elt description 0))) (byte-array->string description))
                   ((arrayp description) (loop for p across description collect (decode-description p)))
                   (t description))))
    (let* ((detail-names '("name" "parameters" "return_type" "can_fail" "deferred"))
           (encoded-names (mapcar #'string-to-octets detail-names))
           (encoded-results (mapcar #'(lambda (n) (gethash n f)) encoded-names)))
      (mapcar #'decode-description encoded-results))))

(defun parse-api (api)
  (let* ((api (elt api 1))
         (functions (gethash (string-to-octets "functions") api)))
    (loop for f across functions
          collect (function-description f))))

(defun retrieve-api ()
  (multiple-value-bind (suc id res) (send-command "vim_get_api_info")
    (declare (ignore suc id))
    res))

(defparameter *api* (parse-api (retrieve-api)))

(mapcar #'(lambda (details) `(create-nvim-func ,@details)) *api*)
