(uiop:define-package #:cave/test
  (:use #:cl
        #:cave
        #:fiveam))
(in-package #:cave/test)

; Root suite, used to trigger all test suites from ASDF
(def-suite cave-suite
           :description "Test my system")

