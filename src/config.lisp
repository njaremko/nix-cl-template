(in-package :cl-user)
(uiop:define-package :cave.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :cave.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root* (asdf:system-source-directory :cave))
(defparameter *static-directory* (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(defconfig :common
           `(:error-log #P"myapp_error.log"
                        :databases (:database-name "cave"
                                                   :username "postgres"
                                                   :password "postgres"
                                                   :host "localhost"
                                                   :port 5432)
                        :auth0 (:domain ,(uiop:getenv "AUTH0_DOMAIN")
                                        :client-id ,(uiop:getenv "AUTH0_CLIENT_ID")
                                        :client-secret ,(uiop:getenv "AUTH0_CLIENT_SECRET")
                                        :redirect-uri "http://localhost:3000/auth/auth0/callback"
                                        :logout-uri "http://localhost:3000")
                        :stripe (:public-key ,(uiop:getenv "STRIPE_PUBLIC_KEY")
                                             :secret-key ,(uiop:getenv "STRIPE_SECRET_KEY")
                                             :webhook-secret ,(uiop:getenv "STRIPE_WEBHOOK_SECRET"))))

(defconfig |development|
           `())


(defconfig |production|
           '())

(defconfig |test|
           '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
