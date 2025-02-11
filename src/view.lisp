(in-package :cl-user)
(uiop:define-package :cave.view
  (:use :cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
  (:import-from :cave.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*
                :*djula-execute-package*)
  (:export :render
           :render-json))
(in-package :cave.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
        template nil
      env)))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (jzon:stringify object))

;;
;; Execute package definition

(defpackage cave.djula
  (:use :cl)
  (:import-from :cave.config
                :config
                :appenv
                :developmentp
                :productionp)
  (:import-from :caveman2
                :url-for))

(setf djula:*template-package* (find-package :cave.djula))
