(load (sb-ext:posix-getenv "ASDF"))

(asdf:load-system :cave)

(uiop:define-package :cave.app
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :cave.web
                :*web*)
  (:import-from :cave.config
                :config
                :productionp
                :*static-directory*))
(in-package :cave.app)

(builder
  (:static
   :path (lambda (path)
           (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
               path
               nil))
   :root *static-directory*)
  (if (productionp)
      nil
      :accesslog)
  (if (getf (config) :error-log)
      `(:backtrace
        :output ,(getf (config) :error-log))
      nil)
  :session
  (if (productionp)
      nil
      (lambda (app)
        (lambda (env)
          (funcall app env))))
  *web*)
