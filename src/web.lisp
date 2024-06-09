(in-package :cl-user)
(uiop:define-package :cave.web
  (:use :cl
        :caveman2
        :cave.config
        :cave.view
        :cave.db)
  (:local-nicknames (#:jzon #:com.inuoe.jzon))
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

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

(defroute "/json" ()
  (let ((a-hash-map (make-hash :INITIAL-CONTENTS '(:x 1 :y 2))))
    (render-json a-hash-map)))

(defroute "/db" ()
  (with-connection
    (let ((fetched (postmodern:query "SELECT 7 + 10 as result, 6 as id")))
      (jzon:stringify fetched))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
