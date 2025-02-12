(in-package :cl-user)
(uiop:define-package :cave.utils
  (:use :cl)
  (:export :example-map
           :example-vector
           :example-set))
(in-package :cave.utils)

#.(cave.reader:enable-clojure-syntax)

(defun example-map ()
  {:a 1, :b 2})

(defun example-vector ()
  [1, 2, 3])

(defun example-set ()
  #{1, 2, 3})
