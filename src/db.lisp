(in-package :cl-user)
(uiop:define-package :cave.db
  (:use :cl)
  (:import-from :cave.config
                :config)
  (:export :with-connection))
(in-package :cave.db)

(defun connection-settings ()
  "Retrieve database connection settings from the application configuration.
   Returns a plist with :database-name, :username, :password, :host, and :port."
  (config :databases))

(defmacro with-connection (&body body)
  "Execute BODY within a database connection context.

   Uses postmodern:with-connection with connection pooling enabled.
   Connection parameters are retrieved from the application configuration.
   The connection is automatically closed when the body exits.

   Example:
   (with-connection
     (postmodern:query \"SELECT * FROM users WHERE id = $1\" id))

   Note: Uses connection pooling (:pooled-p t) for better performance
   in a web application context."
  `(let* ((settings (connection-settings))
          (database-name (getf settings :database-name))
          (username (getf settings :username))
          (password (getf settings :password))
          (host (getf settings :host))
          (port (getf settings :port)))
     (postmodern:with-connection (list database-name username password host :port port :pooled-p t)
       ,@body)))
