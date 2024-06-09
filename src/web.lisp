(in-package :cl-user)
(uiop:define-package :cave.web
  (:use :cl
        :caveman2
        :cave.config
        :cave.view
        :cave.db)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon)
   (#:dex #:dexador))
  (:import-from :make-hash
                :make-hash)
  (:export :*web*))
(in-package :cave.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defparameter *user-info-map* (make-hash-table :test 'equal))

(defun auth0-config (key)
  (getf (config :auth0) key))

;;
;; Routing rules

(defun auth0-login-url ()
  (let* ((domain (auth0-config :domain))
         (client-id (auth0-config :client-id))
         (redirect-uri (auth0-config :redirect-uri)))
    (str:concat "https://" domain "/authorize?"
      "response_type=code&"
      "client_id=" client-id "&"
      "redirect_uri=" redirect-uri "&"
      "scope=openid%20profile%20email")))

(defroute "/" ()
  (let ((login-url (auth0-login-url))
        (session (gethash "auth0" *session*)))
    (render #P"index.html" `(:login-url ,login-url
                                        :session ,session))))

(defroute "/json" ()
  (let ((a-hash-map (make-hash :INITIAL-CONTENTS '(:x 1 :y 2))))
    (render-json a-hash-map)))

(defroute "/db" ()
  (with-connection
    (let ((fetched (postmodern:query "SELECT 7 + 10 as result, 6 as id" :array-hash)))
      (jzon:stringify fetched))))

(defun auth0-logout-url ()
  (let* ((domain (auth0-config :domain))
         (client-id (auth0-config :client-id))
         (logout-uri (auth0-config :logout-uri)))
    (str:concat "https://" domain "/v2/logout?"
      "client_id=" client-id "&"
      "returnTo=" logout-uri)))

(defun store-user-info (user-info)
  (setf (gethash (gethash "sub" user-info) *user-info-map*) user-info))

(defun post-token-request (code)
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
    (dex:post url
      :content body
      :headers `(("Content-Type" . "application/json")))))

(defun parse-response (response)
  (let* ((tokens (jzon:parse response))
         (access-token (gethash "access_token" tokens)))
    (retrieve-user-info access-token)))

(defun retrieve-user-info (access-token)
  (let* ((domain (auth0-config :domain))
         (url (str:concat "https://" domain "/userinfo")))
    (multiple-value-bind (body code headers)
        (dex:get url
          :headers `(("Authorization" . ,(str:concat "Bearer " access-token))))
      (jzon:parse body))))

(defroute "/auth/auth0/callback" (&key |code|)
  (if |code|
      (let* ((response (post-token-request |code|))
             (user-info (parse-response response)))
        (store-user-info user-info)
        (setf (gethash "auth0" *session*) user-info)
        (redirect "/success"))
      (redirect "/failure")))

(defroute "/protected" ()
  (multiple-value-bind (session session-exists) (gethash "auth0" *session*)
    (if session-exists
        (render #P"protected.html" (list (cons :user-info session)))
        (redirect "/login"))))

(defroute "/logout" ()
  (let ((has-session (gethash "auth0" *session*)))
    (if has-session
        (let ((url (auth0-logout-url)))
          (remhash "auth0" *session*)
          (redirect url))
        (redirect "/"))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
