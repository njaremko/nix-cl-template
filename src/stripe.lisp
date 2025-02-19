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
  "Retrieve a configuration value from the Stripe section of the application config.
   KEY should be one of :public-key, :secret-key, or :webhook-secret."
  (getf (config :stripe) key))

(defgeneric raw-body (request)
  (:documentation "Returns the raw body of request as a byte array.
                  This is necessary for webhook signature verification since Stripe signs the raw body."))

(defmethod raw-body ((request lack.request:request))
  "Extract the raw body from a Lack request object. Handles both fixed-length and chunked requests."
  (let* ((env (request-env *request*))
         (len (lack.request:request-content-length request))
         (raw (getf env :raw-body)))
    (if len
        (let ((a (make-array len :element-type '(unsigned-byte 8))))
          (read-sequence a raw)
          a)
        (alexandria:read-stream-content-into-byte-vector raw))))

(defgeneric to-array (ele)
  (:documentation "Convert various types to a byte array for use in cryptographic operations.
                  Supported types: string (converted using UTF-8), array (returned as-is), null (returns nil).")
  (:method ((str string))
           "Convert a string to a UTF-8 encoded byte array."
           (babel:string-to-octets str))
  (:method ((str array))
           "Return the array as-is if it's already a byte array."
           str)
  (:method ((str null))
           "Return nil for null input."
           nil))

(defun compute-signature (signature timestamp raw-body)
  "Compute the HMAC-SHA256 signature of a Stripe webhook payload.

   SIGNATURE - The webhook signing secret key
   TIMESTAMP - Unix timestamp of when the webhook was sent
   RAW-BODY - The raw request body bytes

   Returns a hex-encoded string of the signature for comparison with Stripe's signature."
  (let ((bytes (concatenate '(vector (unsigned-byte 8))
                 (to-array timestamp)
                 (to-array ".")
                 raw-body))
        (hmac (ironclad:make-hmac (to-array signature)
                                  :sha256)))
    (ironclad:update-hmac hmac bytes)
    (ironclad:byte-array-to-hex-string (crypto:hmac-digest hmac))))

(defun verify-signature (signing-secret v1 timestamp raw-body)
  "Verify the authenticity of a Stripe webhook request.

   SIGNING-SECRET - The webhook signing secret from Stripe dashboard
   V1 - The v1 signature from the Stripe-Signature header
   TIMESTAMP - The timestamp from the Stripe-Signature header
   RAW-BODY - The raw request body bytes

   Returns two values:
   1. Boolean indicating if the signature is valid
   2. Time difference in seconds between webhook timestamp and current time

   The time difference can be used to implement additional security checks
   (e.g., rejecting webhooks that are too old)."
  (let* ((ss (to-array signing-secret))
         (genned (compute-signature ss timestamp raw-body))
         (ts (parse-integer timestamp)))
    (values (string= v1 genned)
      (- (local-time:timestamp-to-unix (local-time:now)) ts))))

(defun %validate-stripe-webhook (signing-secret signatures request)
  "Internal function to validate a Stripe webhook request.

   SIGNING-SECRET - The webhook signing secret from Stripe dashboard
   SIGNATURES - The Stripe-Signature header value containing v1 and timestamp
   REQUEST - The raw HTTP request object

   Parses the Stripe-Signature header, extracts the v1 signature and timestamp,
   and validates them against the request body. Raises appropriate errors for
   missing or invalid signature components.

   Returns three values:
   1. Boolean indicating if the signature is valid
   2. Raw request body bytes
   3. Time difference in seconds between webhook timestamp and current time"
  (if (not signatures)
      (error "Missing Stripe signature header")
      (let ((hash (make-hash-table :test #'equal))
            (split (str:split #\, signatures :omit-nulls t)))
        (mapc (lambda (split)
                (destructuring-bind (key val)
                    (str:split #\= split :omit-nulls t)
                  (setf (gethash key hash) val)))
            split)
        (let ((raw (raw-body request))
              (v1-sig (gethash "v1" hash))
              (timestamp (gethash "t" hash)))
          (cond
           ((not v1-sig)
             (error "Missing v1 signature in Stripe webhook"))
           ((not timestamp)
             (error "Missing timestamp in Stripe webhook"))
           (t
             (multiple-value-bind (validp time-dif)
                 (verify-signature signing-secret v1-sig timestamp raw)
               (values validp raw time-dif))))))))

(defmethod verify-webhook ((request LACK.REQUEST:REQUEST))
  "Verify the authenticity of a Stripe webhook request.

   REQUEST - The incoming HTTP request object

   This is the main entry point for webhook verification. It:
   1. Extracts the Stripe-Signature header
   2. Gets the webhook secret from configuration
   3. Validates the webhook signature

   Returns three values:
   1. Boolean indicating if the signature is valid
   2. Raw request body bytes (or nil if validation failed)
   3. Time difference in seconds (or 0 if validation failed)

   Any errors during validation are caught and logged, returning
   (values nil nil 0) to indicate failure."
  (handler-case
      (let* ((headers (request-headers request))
             (signing-secret (stripe-config :webhook-secret))
             (signatures (gethash "stripe-signature" headers)))
        (%validate-stripe-webhook signing-secret signatures request))
    (error (e)
      (format *error-output* "Stripe webhook validation error: ~A~%" e)
      (values nil nil 0))))
