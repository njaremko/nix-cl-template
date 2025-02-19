(in-package :cl-user)
(uiop:define-package :cave.web
  (:use :cl
        :caveman2
        :cave.view
        :cave.db
        :cave.auth)
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

(defmacro defprotected (path args &body body)
  "Define a route that requires authentication.
   If the user is not authenticated (no auth0 session), they will be redirected to /login.
   PATH - The URL path for the route
   ARGS - Arguments to be extracted from the request
   BODY - The route handler body, which will have access to the session variable"
  `(defroute ,path ,args
     (multiple-value-bind (session session-exists) (gethash "auth0" *session*)
       (if session-exists
           (progn ,@body)
           (redirect "/login")))))

;;
;; Routing rules

(defroute "/" ()
  "Home page route. Shows login button if user is not authenticated,
   or a welcome message and logout button if they are."
  (let ((login-url (auth0-login-url))
        (session (gethash "auth0" *session*)))
    (render #P"index.html" `(:login-url ,login-url
                                        :session ,session))))

(defroute "/json" ()
  "Example JSON endpoint that returns a simple hash map.
   Used to demonstrate JSON response handling."
  (let ((a-hash-map (make-hash :INITIAL-CONTENTS '(:x 1 :y 2))))
    (render-json a-hash-map)))

(defroute "/db" ()
  "Example database query endpoint.
   Demonstrates database connection and query execution."
  (with-connection
    (let ((fetched (postmodern:query "SELECT 5 + 19 as result, 6 as id" :array-hash)))
      (jzon:stringify fetched))))

(defroute "/auth/auth0/callback" (&key |code|)
  "Auth0 callback endpoint that handles the OAuth2 authorization code flow.
   Exchanges the authorization code for tokens, retrieves user info,
   and stores it in the session.

   Required query parameter:
   code - The authorization code from Auth0"
  (if |code|
      (let* ((token (exchange-code-for-token |code|))
             (user-info (retrieve-user-info token)))
        (store-user-info user-info)
        (setf (gethash "auth0" *session*) user-info)
        (redirect "/success"))
      (redirect "/failure")))

(defprotected "/protected" ()
  "Protected route that requires authentication.
   Displays user information from the session.
   Will redirect to /login if user is not authenticated."
  (let* ((y (get-user-info (gethash "sub" session)))
         (z (jzon:stringify (fset:convert 'hash-table y))))
    (render #P"protected.html" `((:user-info ,z)))))

(defroute "/logout" ()
  "Logout endpoint that clears the session and redirects to Auth0 logout.
   If user is not logged in, redirects to home page."
  (let ((has-session (gethash "auth0" *session*)))
    (if has-session
        (let ((url (auth0-logout-url)))
          (remhash "auth0" *session*)
          (redirect url))
        (redirect "/"))))

(defroute ("/stripe/webhook" :method :POST) ()
  "Stripe webhook endpoint that validates incoming webhook requests.
   Verifies the webhook signature using the configured webhook secret.
   Returns 401 if signature is invalid, 200 if valid."
  (let ((is-valid (cave.stripe:verify-webhook *request*)))
    (if is-valid
        "Ok"
        (throw-code 401))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  "Custom 404 error page handler."
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   cave.config:*template-directory*))
