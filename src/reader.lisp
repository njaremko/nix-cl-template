(in-package :cl-user)
(uiop:define-package :cave.reader
  (:use :cl :named-readtables)
  (:import-from :fset
                :empty-map
                :empty-seq
                :with
                :seq)
  (:export :enable-clojure-syntax
           :disable-clojure-syntax))
(in-package :cave.reader)

;; Reader macro for Clojure-style vectors using [...]
(defun replace-booleans (obj)
  (cond
    ((and (symbolp obj) (string-equal (symbol-name obj) "TRUE")) t)
    ((and (symbolp obj) (string-equal (symbol-name obj) "FALSE")) nil)
    ((fset:seq? obj) (fset:image #'replace-booleans obj))
    ((fset:map? obj) (fset:image (lambda (k v)
                                  (values (replace-booleans k)
                                          (replace-booleans v)))
                                obj))
    ((fset:set? obj) (fset:image #'replace-booleans obj))
    (t obj)))

(defun read-vector (stream char)
  (declare (ignore char))
  (fset:convert 'fset:seq (mapcar #'replace-booleans (read-delimited-list #\] stream t))))

;; Reader macro for Clojure-style maps using {...}
(defun read-map (stream char)
  (declare (ignore char))
  (let* ((elements (mapcar #'replace-booleans (read-delimited-list #\} stream t)))
         (map (fset:empty-map)))
    (when (oddp (length elements))
      (error "Map literal must contain an even number of elements"))

    (loop for (k v) on elements by #'cddr do
      (unless (keywordp k)
        (error "Map keys must be keywords"))
      (setf map (fset:with map k v)))

    map))

;; Reader macro for Clojure-style sets using #{...}
(defun read-set (stream char arg)
  (declare (ignore char arg))  ; Accept all 3 parameters but ignore the latter two.
  (fset:convert 'fset:set (mapcar #'replace-booleans (read-delimited-list #\} stream t))))

;; Define the readtable in a global variable.
(defreadtable *clojure-syntax*
  (:merge :standard)
  ;; Treat comma as whitespace by copying space's syntax
  (:syntax-from :standard #\Space #\,)
  ;; Only define macros for the opening delimiters.
  (:macro-char #\[ 'read-vector)
  (:macro-char #\{ 'read-map)
  (:macro-char #\} (get-macro-character #\)))
  (:macro-char #\] (get-macro-character #\)))
  (:dispatch-macro-char #\# #\{ 'read-set))

(defun enable-clojure-syntax ()
  "Enable Clojure-style literal syntax for vectors, maps, and sets."
  (in-readtable *clojure-syntax*))

(defun disable-clojure-syntax ()
  "Disable Clojure-style syntax by removing custom literal macros.
Any attempt to read a vector ([...]), map ({...}), or set (#{...}) will now signal an error."
  (in-readtable :standard))
