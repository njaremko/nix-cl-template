(in-package :cl-user)
(uiop:define-package :cave.utils
  (:use :cl)
  (:export :example-map
           :example-vector
           :example-set
           :generate-context-file))
(in-package :cave.utils)

#.(cave.reader:enable-clojure-syntax)

(defun example-map ()
  {:a 1, :b 2})

(defun example-vector ()
  [1, 2, 3])

(defun example-set ()
  #{1, 2, 3})

(defun extract-component-files (component)
  "Recursively extracts files from an ASDF component"
  (typecase component
    (asdf:source-file (list (namestring (asdf:component-pathname component))))
    (asdf:module
     (loop for c in (asdf:component-children component)
             append (extract-component-files c)))
    (t nil)))

(defun generate-context-file ()
  "Extracts all source files from a system in load order and concatenates their contents"
  (handler-case
      (let* ((system-name "cave")
             (output-file #p"context.lisp")
             (system (asdf:find-system system-name))
             (files (extract-component-files system)))
        (with-open-file (out output-file :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (dolist (file files)
            ;; Write a header comment for each file
            (format out "~%;;;; Contents of ~A~%~%" file)
            ;; Copy the file contents
            (with-open-file (in file)
              (loop for line = (read-line in nil nil)
                    while line
                    do (write-line line out)))))
        (format t "~&Successfully concatenated files to ~A~%" output-file)
        files)
    (asdf:missing-component (c)
                            (format t "~&Error: System '~A' not found~%" c))
    (error (c)
      (format t "~&Error: ~A~%" c))))
