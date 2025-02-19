(in-package :cl-user)
(uiop:define-package :cave.auth
  (:use :cl
        :caveman2
        :cave.config
        :cave.db)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon)
   (#:dex #:dexador))
  (:import-from :make-hash
                :make-hash)
  (:export :auth0-login-url
           :auth0-logout-url
           :exchange-code-for-token
           :retrieve-user-info
           :store-user-info
           :get-user-info))
(in-package :cave.auth)

(defparameter *user-info-map* (fset:empty-map))

(declaim (ftype (function (symbol) string) auth0-config))
(defun auth0-config (key)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (getf (config :auth0) key))

(declaim (ftype (function () string) auth0-login-url))
(defun auth0-login-url ()
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((domain (auth0-config :domain))
         (client-id (auth0-config :client-id))
         (redirect-uri (auth0-config :redirect-uri)))
    (str:concat "https://" domain "/authorize?"
      "response_type=code&"
      "client_id=" client-id "&"
      "redirect_uri=" redirect-uri "&"
      "scope=openid%20profile%20email")))

(declaim (ftype (function () string) auth0-logout-url))
(defun auth0-logout-url ()
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((domain (auth0-config :domain))
         (client-id (auth0-config :client-id))
         (logout-uri (auth0-config :logout-uri)))
    (str:concat "https://" domain "/v2/logout?"
      "client_id=" client-id "&"
      "returnTo=" logout-uri)))

(declaim (ftype (function (string) string) exchange-code-for-token))
(defun exchange-code-for-token (code)
  (let* ((domain (auth0-config :domain))
         (client-id (auth0-config :client-id))
         (client-secret (auth0-config :client-secret))
         (redirect-uri (auth0-config :redirect-uri))
         (url (str:concat "https://" domain "/oauth/token"))
         (body (jzon:stringify
                 (fset:convert 'hash-table
                               (fset:map (:grant_type "authorization_code")
                                 (:client_id client-id)
                                 (:client_secret client-secret)
                                 (:code code)
                                 (:redirect_uri redirect-uri))))))
    (let* ((response (dex:post url
                       :content body
                       :headers `(("Content-Type" . "application/json"))))
           (tokens (jzon:parse response)))
      (gethash "access_token" tokens))))

(declaim (ftype (function (string) (values hash-table &optional)) retrieve-user-info))
(defun retrieve-user-info (access-token)
  (let* ((domain (auth0-config :domain))
         (url (str:concat "https://" domain "/userinfo")))
    (jzon:parse (dex:get url
                  :headers `(("Authorization" . ,(str:concat "Bearer " access-token)))))))

(defun store-user-info (user-info)
  (setf *user-info-map* (fset:with *user-info-map* (gethash "sub" user-info) (fset:convert 'fset:map user-info))))

(declaim (ftype (function (string) (values (or null fset:map) &optional)) get-user-info))
(defun get-user-info (sub)
  (fset:lookup *user-info-map* sub))
