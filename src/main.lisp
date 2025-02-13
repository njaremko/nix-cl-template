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
        (asdf:system-relative-pathname :cave #P"app.lisp"))

(defvar *handler* nil)
(defvar *main-thread* nil)

(defun start-custom (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (when *handler*
        (restart-case (error "Server is already running.")
          (restart-server ()
                          :report "Restart the server"
                          (stop))))
  (setf *main-thread* (bt:current-thread))
  (setf *handler*
    (apply #'clackup *appfile-path* args)))

(defun start ()
  (start-custom :server :woo :port 3000))

(defun start-dev ()
  (start-custom :server :hunchentoot :port 3000 :debug t))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))
