(in-package #:nvim)

(defun encode-name (name)
  (loop for b across (string-to-octets name) collect b))

(defun function-metadata (f)
  (labels ((decode-metadata (metadata)
             (cond ((and (listp metadata) (= (length metadata) 0)) '())
                   ((and (listp metadata) (numberp (first metadata))) (byte-array->string metadata))
                   ((listp metadata) (loop for p in metadata collect (decode-metadata p)))
                   (t metadata))))
    (let* ((metadata-names '("name" "parameters" "return_type" "can_fail" "deferred"))
           (encoded-names (mapcar #'encode-name metadata-names))
           (encoded-results (mapcar #'(lambda (n) (gethash n f)) encoded-names)))
      (mapcar #'decode-metadata encoded-results))))

(defun parse-api (api)
  (let* ((api (second api))
         (functions (gethash (encode-name "functions") api)))
    (loop for f in functions
          collect (function-metadata f))))

(defun retrieve-api ()
  (nvim:api-info))

(defparameter *api* (parse-api (retrieve-api)))

(mapcar #'(lambda (metadata) `(mdata->lisp-function ,@metadata)) *api*)
