(load (sb-ext:posix-getenv "ASDF"))

(asdf:load-system :cave)

(uiop:define-package :cave.app
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :cave.web
                :*web*)
  (:import-from :cave.config
                :config
                :production-p
                :*static-directory*))
(in-package :cave.app)

(builder
  (:static
   :path (lambda (path)
           (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
               path
               nil))
   :root *static-directory*)
  (if (production-p)
      nil
      :accesslog)
  (if (getf (config) :error-log)
      `(:backtrace
        :output ,(getf (config) :error-log))
      nil)
  :session
  (if (production-p)
      (lambda (app)
        (lambda (env)
          (handler-case
              (funcall app env)
            (error (e)
              (declare (ignore e))
              (let* ((headers (getf env :headers))
                     (accept (gethash "accept" headers))
                     (accepts-json (and accept (search "application/json" accept))))
                (if accepts-json
                    `(500 (:content-type "application/json")
                          (,(format nil "{\"error\": \"Stripe Webhook Error\", \"message\": \"~A\"}" (princ-to-string e))))
                    `(500 (:content-type "text/html")
                          ("<html><body><h1>Internal Server Error</h1><p>Stripe Webhook Error: ~A</p></body></html>" (princ-to-string e)))))))))
      (lambda (app)
        (lambda (env)
          (funcall app env))))
  *web*)
