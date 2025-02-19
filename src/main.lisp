(in-package :cl-user)
(uiop:define-package :cave
  (:use :cl)
  (:import-from :cave.config
                :config)
  (:import-from :clack
                :clackup)
  (:import-from :bordeaux-threads
                :current-thread
                :interrupt-thread)
  (:export :start
           :start-dev
           :start-custom
           :stop))
(in-package :cave)

(defvar *appfile-path*
        (asdf:system-relative-pathname :cave #P"app.lisp")
        "Path to the main application file.")

(defvar *handler* nil
        "The current server handler instance. NIL when server is not running.")

(defun start-custom (&rest args &key server port debug swank-port &allow-other-keys)
  "Start the server with custom configuration.

   Args:
     :SERVER - The server implementation to use (e.g. :woo, :hunchentoot)
     :PORT - The port to listen on
     :DEBUG - Whether to enable debug mode
     :SWANK-PORT - Port for Swank server (when debugging)

   Signals an error if server is already running, but provides a restart
   to stop the current server and start a new one."
  (declare (ignore server port debug swank-port))
  (when *handler*
        (restart-case (error "Server is already running on thread ~A." *main-thread*)
          (restart-server ()
                          :report "Stop the current server and start a new one"
                          (stop))))
  (handler-case
      (setf *handler*
        (apply #'clackup *appfile-path* args))
    (error (e)
      (error "Failed to start server: ~A" e))))

(defun start ()
  (start-custom :server :woo :port 3000 :debug nil))

(defun start-dev ()
  (start-custom :server :hunchentoot :port 3000 :swank-port 4005 :debug t))

(defun stop ()
  "Stop the server if it is running.
   Returns T if server was stopped, NIL if no server was running."
  (when *handler*
        (handler-case
            (prog1 t
              (clack:stop *handler*)
              (setf *handler* nil))
          (error (e)
            (warn "Error while stopping server: ~A" e)
            (setf *handler* nil)
            nil))))
