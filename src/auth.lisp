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

;;; User info storage is intentionally ephemeral, used only for temporary session data.
;;; In a production environment with multiple server instances, this should be replaced
;;; with a persistent store (e.g., Redis, database) if user data needs to persist
;;; across requests or be shared between instances.
(defparameter *user-info-map* (fset:empty-map))

(defun auth0-config (key)
  "Retrieve a configuration value from the Auth0 section of the application config.
   KEY should be one of :domain, :client-id, :client-secret, :redirect-uri, or :logout-uri."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (getf (config :auth0) key))

(declaim (ftype (function () string) auth0-login-url))
(defun auth0-login-url ()
  "Generate the Auth0 login URL for initiating the OAuth2 authorization code flow.
   Includes response_type, client_id, redirect_uri, and required scopes.
   The URL will redirect the user to Auth0's hosted login page."
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
  "Generate the Auth0 logout URL.
   This URL will clear the Auth0 session and redirect back to the application.
   Uses the configured logout URI as the return-to URL."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((domain (auth0-config :domain))
         (client-id (auth0-config :client-id))
         (logout-uri (auth0-config :logout-uri)))
    (str:concat "https://" domain "/v2/logout?"
      "client_id=" client-id "&"
      "returnTo=" logout-uri)))

(declaim (ftype (function (string) string) exchange-code-for-token))
(defun exchange-code-for-token (code)
  "Exchange an OAuth2 authorization code for an access token.

   CODE - The authorization code received from Auth0

   Makes a POST request to Auth0's token endpoint with the authorization code,
   client credentials, and redirect URI. Returns the access token from the response.

   Note: The access token is short-lived and should be used immediately to retrieve
   user information."
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
  "Retrieve the user's profile information from Auth0.

   ACCESS-TOKEN - A valid access token obtained from exchange-code-for-token

   Makes a GET request to Auth0's userinfo endpoint with the access token.
   Returns a hash table containing the user's profile information (sub, email, etc.)."
  (let* ((domain (auth0-config :domain))
         (url (str:concat "https://" domain "/userinfo")))
    (jzon:parse (dex:get url
                  :headers `(("Authorization" . ,(str:concat "Bearer " access-token)))))))

(defun store-user-info (user-info)
  "Store user information in the ephemeral user info map.

   USER-INFO - Hash table containing user profile information from Auth0

   The user info is stored in an fset:map, keyed by the user's 'sub' (subject) identifier.
   Note: This storage is ephemeral and will be cleared when the server restarts.
   For production use with multiple server instances, consider using a persistent store."
  (setf *user-info-map* (fset:with *user-info-map* (gethash "sub" user-info) (fset:convert 'fset:map user-info))))

(declaim (ftype (function (string) (values (or null fset:map) &optional)) get-user-info))
(defun get-user-info (sub)
  "Retrieve stored user information by subject identifier.

   SUB - The subject identifier from Auth0 (unique user ID)

   Returns the user's profile information as an fset:map, or NIL if not found.
   Note: This retrieves from ephemeral storage. In a production environment
   with multiple server instances, this should be replaced with a persistent store lookup."
  (fset:lookup *user-info-map* sub))
