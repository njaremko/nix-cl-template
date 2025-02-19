(in-package :cl-user)
(uiop:define-package :cave.session
  (:use :cl)
  (:import-from :cave.config
                :config
                :production-mode-p)
  (:import-from :bordeaux-threads
                :make-lock
                :with-lock-held)
  (:import-from :cl-redis
                :connect
                :disconnect
                :with-connection
                :set
                :get
                :del)
  (:export :with-session
           :session-get
           :session-set
           :session-delete
           :make-session-store
           :clear-session))
(in-package :cave.session)

;;; Session Store Protocol

(defgeneric session-store-get (store key))
(defgeneric session-store-set (store key value))
(defgeneric session-store-delete (store key))
(defgeneric session-store-clear (store))

;;; In-Memory Session Store

(defclass memory-session-store ()
          ((data :initform (make-hash-table :test 'equal)
                 :reader store-data)
           (lock :initform (make-lock "session-store-lock")
                 :reader store-lock)))

(defmethod session-store-get ((store memory-session-store) key)
           (with-lock-held ((store-lock store))
             (gethash key (store-data store))))

(defmethod session-store-set ((store memory-session-store) key value)
           (with-lock-held ((store-lock store))
             (setf (gethash key (store-data store)) value)))

(defmethod session-store-delete ((store memory-session-store) key)
           (with-lock-held ((store-lock store))
             (remhash key (store-data store))))

(defmethod session-store-clear ((store memory-session-store))
           (with-lock-held ((store-lock store))
             (clrhash (store-data store))))

;;; Redis Session Store

(defclass redis-session-store ()
          ((connection-spec :initarg :connection-spec
                            :reader connection-spec)))

(defun parse-redis-url (url)
       "Parse Redis URL into connection parameters."
       (let* ((uri (quri:uri url))
              (host (quri:uri-host uri))
              (port (quri:uri-port uri))
              (password (quri:uri-userinfo uri)))
             (list :host (or host "localhost")
                   :port (or port 6379)
                   :password password)))

(defmethod session-store-get ((store redis-session-store) key)
           (with-connection ((connection-spec store))
                            (let ((value (get key)))
                                 (when value
                                       (read-from-string value)))))

(defmethod session-store-set ((store redis-session-store) key value)
           (with-connection ((connection-spec store))
                            (set key (prin1-to-string value))))

(defmethod session-store-delete ((store redis-session-store) key)
           (with-connection ((connection-spec store))
                            (del key)))

(defmethod session-store-clear ((store redis-session-store))
           (with-connection ((connection-spec store))
                            (del "*")))

;;; Session Store Factory

(defvar *session-store* nil
        "The current session store instance.")

(defun make-session-store ()
       "Create a new session store based on configuration."
       (setf *session-store*
             (ecase (config :session-store)
                    (:memory (make-instance 'memory-session-store))
                    (:redis (make-instance 'redis-session-store
                                           :connection-spec
                                           (parse-redis-url (config :redis-url)))))))

;;; Public Interface

(defmacro with-session (() &body body)
          "Execute BODY with session store bound."
          `(progn
            (unless *session-store*
                    (make-session-store))
            ,@body))

(defun session-get (key)
       "Get value for KEY from session store."
       (with-session ()
                     (session-store-get *session-store* key)))

(defun session-set (key value)
       "Set VALUE for KEY in session store."
       (with-session ()
                     (session-store-set *session-store* key value)))

(defun session-delete (key)
       "Delete KEY from session store."
       (with-session ()
                     (session-store-delete *session-store* key)))

(defun clear-session ()
       "Clear all session data."
       (with-session ()
                     (session-store-clear *session-store*)))

;; Initialize session store on load if in production
(when (production-mode-p)
      (make-session-store))
