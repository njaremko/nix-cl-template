(in-package :cl-user)
(uiop:define-package :cave.db
  (:use :cl)
  (:import-from :cave.config
                :config)
  (:import-from :postmodern
                :connect-cached)
  (:export :with-connection))
(in-package :cave.db)

(defun connection-settings (&optional (db :maindb))
    (config :databases))

(defmacro with-connection (&body body)
  `(let* ((settings (connection-settings))
          (database-name (getf settings :database-name))
          (username (getf settings :username))
          (password (getf settings :password))
          (host (getf settings :host))
          (port (getf settings :port)))
     (postmodern:with-connection (list database-name username password host :port port :pooled-p t)
       ,@body)))
