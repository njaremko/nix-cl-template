(uiop:define-package #:cave/tests/main
  (:use #:cl
        #:cave
        #:rove))
(in-package #:cave/tests/main)

(deftest parse-message-test
  (testing "invalid message"
    (ok (= 10 10))))
