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
           :store-user-info))
(in-package :cave.auth)

(defparameter *user-info-map* (make-hash-table :test 'equal))

(defun auth0-config (key)
  (getf (config :auth0) key))

(defun auth0-login-url ()
  (let* ((domain (auth0-config :domain))
         (client-id (auth0-config :client-id))
         (redirect-uri (auth0-config :redirect-uri)))
    (str:concat "https://" domain "/authorize?"
      "response_type=code&"
      "client_id=" client-id "&"
      "redirect_uri=" redirect-uri "&"
      "scope=openid%20profile%20email")))

(defun auth0-logout-url ()
  (let* ((domain (auth0-config :domain))
         (client-id (auth0-config :client-id))
         (logout-uri (auth0-config :logout-uri)))
    (str:concat "https://" domain "/v2/logout?"
      "client_id=" client-id "&"
      "returnTo=" logout-uri)))

(defun exchange-code-for-token (code)
  (let* ((domain (auth0-config :domain))
         (client-id (auth0-config :client-id))
         (client-secret (auth0-config :client-secret))
         (redirect-uri (auth0-config :redirect-uri))
         (url (str:concat "https://" domain "/oauth/token"))
         (body (jzon:stringify
                 (make-hash :INITIAL-CONTENTS `(:grant_type "authorization_code"
                                                            :client_id ,client-id
                                                            :client_secret ,client-secret
                                                            :code ,code
                                                            :redirect_uri ,redirect-uri)))))
    (multiple-value-bind (body code)
        (dex:post url
          :content body
          :headers `(("Content-Type" . "application/json")))
      (if (= code 200)
          (let ((tokens (jzon:parse body)))
            (gethash "access_token" tokens))
          (redirect "/failure")))))

(defun retrieve-user-info (access-token)
  (let* ((domain (auth0-config :domain))
         (url (str:concat "https://" domain "/userinfo")))
    (multiple-value-bind (body code)
        (dex:get url
          :headers `(("Authorization" . ,(str:concat "Bearer " access-token))))
      (if (= code 200)
          (jzon:parse body)
          (redirect "/failure")))))

(defun store-user-info (user-info)
  (setf (gethash (gethash "sub" user-info) *user-info-map*) user-info))
