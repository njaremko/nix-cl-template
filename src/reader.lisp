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

;;;; Custom Reader Macros for Clojure-like Data Structures
;;;
;;; This module provides a set of reader macros that allow for Clojure-style
;;; literal syntax in Common Lisp code. The following syntax is supported:
;;;
;;; - Vectors: [1 2 3] creates an fset:seq
;;; - Maps: {:a 1 :b 2} creates an fset:map
;;; - Sets: #{1 2 3} creates an fset:set
;;;
;;; Boolean values TRUE and FALSE (case-insensitive) are automatically
;;; converted to t and nil respectively.
;;;
;;; Example usage:
;;;   [1 2 3]              => #<Seq {1 2 3}>
;;;   {:a 1 :b 2}         => #<Map {:a 1 :b 2}>
;;;   #{1 2 3}            => #<Set {1 2 3}>
;;;   [TRUE FALSE]        => #<Seq {t nil}>
;;;
;;; Use (enable-clojure-syntax) to enable these reader macros in your code.
;;; Use (disable-clojure-syntax) to return to standard Common Lisp syntax.

;; Reader macro for Clojure-style vectors using [...]
(defun replace-booleans (obj)
  "Recursively traverse data structures and replace 'TRUE' and 'FALSE' symbols with t and nil.
   Works with fset sequences, maps, and sets. Case-insensitive comparison is used for boolean values."
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
  "Reader macro function for Clojure-style vector syntax [a b c].
   Creates an fset:seq from the elements between [ and ].
   Automatically converts TRUE/FALSE to t/nil."
  (declare (ignore char))
  (fset:convert 'fset:seq (mapcar #'replace-booleans (read-delimited-list #\] stream t))))

;; Reader macro for Clojure-style maps using {...}
(defun read-map (stream char)
  "Reader macro function for Clojure-style map syntax {:a 1 :b 2}.
   Creates an fset:map from paired elements between { and }.
   Keys must be hashable (keyword, string, number, symbol, or fset data structure).
   Automatically converts TRUE/FALSE to t/nil."
  (declare (ignore char))
  (let* ((elements (mapcar #'replace-booleans (read-delimited-list #\} stream t)))
         (map (fset:empty-map)))
    (when (oddp (length elements))
          (error "Map literal must contain an even number of elements"))

    (loop for (k v) on elements by #'cddr do
            (unless (typep k '(or keyword string number symbol fset:seq fset:map fset:set))
              (error "Map keys must be hashable (keyword, string, number, symbol, or fset data structure)"))
            (setf map (fset:with map k v)))

    map))

;; Reader macro for Clojure-style sets using #{...}
(defun read-set (stream char &rest args)
  "Reader macro function for Clojure-style set syntax #{a b c}.
   Creates an fset:set from the elements between #{ and }.
   Automatically converts TRUE/FALSE to t/nil."
  (declare (ignore char args))
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
  (:dispatch-macro-char #\# #\{ 'read-set)
  ;; Add Clojure-style comment syntax
  (:dispatch-macro-char #\# #\_
                        (lambda (stream char arg)
                          (declare (ignore char arg))
                          (read stream nil (values) t)
                          (values))))

(defun enable-clojure-syntax ()
  "Enable Clojure-style literal syntax for vectors, maps, and sets."
  (in-readtable *clojure-syntax*))

(defun disable-clojure-syntax ()
  "Disable Clojure-style syntax by removing custom literal macros.
Any attempt to read a vector ([...]), map ({...}), or set (#{...}) will now signal an error."
  (in-readtable :standard))
