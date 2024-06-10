(in-package :cl-user)
(uiop:define-package :cave.web
  (:use :cl
        :caveman2
        :cave.config
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
  `(defroute ,path ,args
     (multiple-value-bind (session session-exists) (gethash "auth0" *session*)
       (if session-exists
           (progn ,@body)
           (redirect "/login")))))

;;
;; Routing rules

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
    (let ((fetched (postmodern:query "SELECT 5 + 19 as result, 6 as id" :array-hash)))
      (jzon:stringify fetched))))

(defroute "/auth/auth0/callback" (&key |code|)
  (if |code|
      (let* ((token (exchange-code-for-token |code|))
             (user-info (retrieve-user-info token)))
        (store-user-info user-info)
        (setf (gethash "auth0" *session*) user-info)
        (redirect "/success"))
      (redirect "/failure")))

(defprotected "/protected" ()
  (render #P"protected.html" (list (cons :user-info session))))

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
