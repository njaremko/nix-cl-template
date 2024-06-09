
(in-package #:cave/test)

(def-suite example-suite
           :description "Example of a possible suite"
           :in cave-suite)
(in-suite example-suite)

(test simple-test
      (is (= 10 10)))
