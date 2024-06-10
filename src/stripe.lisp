(in-package :cl-user)
(uiop:define-package :cave.stripe
  (:use :cl
        :caveman2
        :cave.config)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon)
   (#:dex #:dexador))
  (:import-from :make-hash
                :make-hash)
  (:export :verify-webhook))
(in-package :cave.stripe)

(defun stripe-config (key)
  (getf (config :stripe) key))

(defgeneric raw-body (request)
  (:documentation "Returns the raw body of request."))

(defmethod raw-body ((request lack.request:request))
  (let* ((env (request-env *request*))
         (len (lack.request:request-content-length request))
         (raw (getf env :raw-body)))
    (if len
        (let ((a (make-array len :element-type '(unsigned-byte 8))))
          (read-sequence a raw)
          a)
        (alexandria:read-stream-content-into-byte-vector raw))))

(defgeneric to-array (ele)
  (:documentation "Convert ELE to an array")
  (:method ((str string))
           (babel:string-to-octets str))
  (:method ((str array))
           str)
  (:method ((str null))
           nil))

(defun compute-signature (signature timestamp raw-body)
  "Convert everything that is not an array to an array and then compute a hmac. Return
a hex string as the final result."
  (let ((bytes (concatenate '(vector (unsigned-byte 8))
                 (to-array timestamp)
                 (to-array ".")
                 raw-body))
        (hmac (ironclad:make-hmac (to-array signature)
                                  :sha256)))
    (ironclad:update-hmac hmac bytes)
    (ironclad:byte-array-to-hex-string (crypto:hmac-digest hmac))))

(defun verify-signature (signing-secret v1 timestamp raw-body)
  "Verifies the received V1 using TIMESTAMP and RAW-BODY. Returns whether it is
valid (bool) and the difference between TIMESTAMP and #'local-time:now (unix epoch time)"
  (let* ((ss (to-array signing-secret))
         (genned (compute-signature ss timestamp raw-body))
         (ts (parse-integer timestamp)))
    (values (string= v1 genned)
      (- (local-time:timestamp-to-unix (local-time:now)) ts))))

(defun %validate-stripe-webhook (signing-secret signatures request)
  (let ((hash (make-hash-table :test #'equal))
        (split (str:split #\, signatures :omit-nulls t)))
    (mapc (lambda (split)
            (destructuring-bind (key val)
                (str:split #\= split :omit-nulls t)
              (setf (gethash key hash) val)))
        split)
    (let ((raw (raw-body request)))
      (multiple-value-bind (validp time-dif)
          (verify-signature signing-secret (gethash "v1" hash) (gethash "t" hash) raw)
        (values validp raw time-dif)))))

(defmethod verify-webhook ((request LACK.REQUEST:REQUEST))
  (let* ((headers (request-headers request))
         (signing-secret (stripe-config :webhook-secret))
         (signatures (gethash "stripe-signature" headers)))
    (%validate-stripe-webhook signing-secret signatures request)))
