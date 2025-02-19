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

(defparameter *template-registry* (make-hash-table :test 'equal)
              "Registry of compiled Djula templates. Templates are compiled once and cached here
   for improved performance. Keys are template paths, values are compiled templates.")

(defun render (template-path &optional env)
  "Render a template using the Djula templating system.

   TEMPLATE-PATH - Path to the template file relative to the template directory
   ENV - Optional environment/variables to pass to the template

   Templates are compiled on first use and cached in *template-registry*.
   The template is rendered with the provided environment variables.

   Example:
   (render #P\"index.html\" '(:title \"Welcome\" :user user-object))"
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
        template nil
      env)))

(defun render-json (object)
  "Render an object as a JSON response.
   Sets the Content-Type header to application/json and
   converts the object to JSON using jzon:stringify.

   OBJECT - Any Lisp object that can be serialized to JSON

   Example:
   (render-json '(:status \"success\" :data (:id 1 :name \"test\")))"
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (jzon:stringify object))

;;
;; Execute package definition

(defpackage cave.djula
  (:use :cl)
  (:import-from :cave.config
                :config
                :appenv
                :development-p
                :production-p)
  (:import-from :caveman2
                :url-for))

(setf djula:*template-package* (find-package :cave.djula))
