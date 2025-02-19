(in-package :cl-user)
(uiop:define-package :cave.config
  (:use :cl)
  (:export :config
           :*environment*
           :*application-root*
           :*static-directory*
           :*template-directory*
           :development-p
           :production-p))
(in-package :cave.config)

(defparameter *application-root* (asdf:system-source-directory :cave)
              "Root directory of the application.")

(defparameter *static-directory* (merge-pathnames #P"static/" *application-root*)
              "Directory for static files.")

(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*)
              "Directory for Djula templates.")

(defvar *environment* :development
        "Current environment (:development or :production).")

(defun development-p ()
  "Returns T if running in development mode."
  (eq *environment* :development))

(defun production-p ()
  "Returns T if running in production mode."
  (eq *environment* :production))

(defvar *config-values* (make-hash-table :test 'equal)
        "Storage for configuration values.")

(defvar *default-config*
        '((:development
           (:database-url . "postgres://localhost:5432/cave_dev")
           (:redis-url . "redis://localhost:6379")
           (:port . 3000)
           (:debug . t)
           (:session-store . :memory)
           (:log-level . :debug))
          (:production
           (:database-url . nil) ; Must be set in production
           (:redis-url . nil) ; Must be set in production
           (:port . 3000)
           (:debug . nil)
           (:session-store . :redis)
           (:log-level . :info)))
        "Default configuration values for different environments.")

(defun safe-getenv (name &optional default)
  "Get environment variable NAME, returning DEFAULT if not found.
   Signals an error in production if no default is provided and the variable is missing."
  (let ((value (uiop:getenv name)))
    (cond
     (value value)
     ((and (null default) (production-p))
       (error "Missing required environment variable in production: ~A" name))
     (t default))))

(defun load-config-from-env ()
  "Load configuration from environment variables."
  (setf *environment*
    (if (string-equal (safe-getenv "APP_ENV" "development") "production")
        :production
        :development))

  (let ((defaults (cdr (assoc *environment* *default-config*))))
    ;; Load each config value, preferring env vars over defaults
    (setf (gethash "database-url" *config-values*)
      (safe-getenv "DATABASE_URL" (cdr (assoc :database-url defaults))))

    (setf (gethash "redis-url" *config-values*)
      (safe-getenv "REDIS_URL" (cdr (assoc :redis-url defaults))))

    (setf (gethash "port" *config-values*)
      (parse-integer (safe-getenv "PORT" (write-to-string (cdr (assoc :port defaults))))))

    (setf (gethash "debug" *config-values*)
      (string-equal (safe-getenv "DEBUG" (if (cdr (assoc :debug defaults)) "true" "false"))
                    "true"))

    (setf (gethash "session-store" *config-values*)
      (intern (string-upcase (safe-getenv "SESSION_STORE"
                                          (string (cdr (assoc :session-store defaults)))))
              :keyword))

    (setf (gethash "log-level" *config-values*)
      (intern (string-upcase (safe-getenv "LOG_LEVEL"
                                          (string (cdr (assoc :log-level defaults)))))
              :keyword))
    ;; Auth0 configuration (required in production)
    (setf (gethash "auth0" *config-values*)
      (list :domain (safe-getenv "AUTH0_DOMAIN")
            :client-id (safe-getenv "AUTH0_CLIENT_ID")
            :client-secret (safe-getenv "AUTH0_CLIENT_SECRET")
            :redirect-uri (safe-getenv "AUTH0_REDIRECT_URI")
            :logout-uri (safe-getenv "AUTH0_LOGOUT_URI")))

    ;; Stripe configuration (required in production)
    (setf (gethash "stripe" *config-values*)
      (list :public-key (safe-getenv "STRIPE_PUBLIC_KEY")
            :secret-key (safe-getenv "STRIPE_SECRET_KEY")
            :webhook-secret (safe-getenv "STRIPE_WEBHOOK_SECRET")))))

(defun config (key)
  "Get configuration value for KEY.
   KEY should be a string or keyword.
   Returns NIL if the key is not found."
  (gethash (string-downcase (string key)) *config-values*))

;; Load configuration when the file is loaded
(load-config-from-env)
