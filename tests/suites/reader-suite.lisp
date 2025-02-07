(in-package #:cave/test)

(def-suite reader-suite
           :description "Tests for Clojure-style reader macros"
           :in cave-suite)
(in-suite reader-suite)

(test vector-reader-test
      (cave.reader:enable-clojure-syntax)
      (let ((result (read-from-string "[1 2 3]")))
        (is (typep result 'fset:seq))
        (is (= 3 (fset:size result)))
        (is (equal '(1 2 3)
                  (fset:convert 'list result)))))

(test map-reader-test
      (cave.reader:enable-clojure-syntax)
      (let ((result (read-from-string "{:a 1 :b 2}")))
        (is (typep result 'fset:map))
        (is (= 2 (fset:size result)))
        (is (= 1 (fset:@ result :a)))
        (is (= 2 (fset:@ result :b)))))

(test set-reader-test
      (cave.reader:enable-clojure-syntax)
      (let ((result (read-from-string "#{1 2 3}")))
        (is (typep result 'fset:set))
        (is (= 3 (fset:size result)))
        (is (fset:equal? result (fset:set 1 2 3))))
      ;; Test empty set
      (let ((empty-set (read-from-string "#{}")))
        (is (zerop (fset:size empty-set)))))

(test nested-structures-test
      (cave.reader:enable-clojure-syntax)
      (let ((result (read-from-string "[{:a [1 2] :b #{3}} :foo {:x 'y}]")))
        (is (typep (fset:lookup result 0) 'fset:map))
        (is (typep (fset:lookup (fset:lookup result 0) :a) 'fset:seq))
        (is (typep (fset:lookup (fset:lookup result 0) :b) 'fset:set))
        (is (fset:contains? (fset:lookup (fset:lookup result 0) :b) 3))
        (is (typep (fset:lookup result 2) 'fset:map))
        (is (eq (fset:lookup result 1) :foo)))
      ;; Test empty nested collections
      (let ((result2 (read-from-string "[{} [] #{}]")))
        (is (typep (fset:lookup result2 0) 'fset:map))
        (is (typep (fset:lookup result2 1) 'fset:seq))
        (is (typep (fset:lookup result2 2) 'fset:set))
        (is (zerop (fset:size (fset:lookup result2 0))))
        (is (zerop (fset:size (fset:lookup result2 1))))
        (is (zerop (fset:size (fset:lookup result2 2))))))

(test mixed-data-types-test
      (cave.reader:enable-clojure-syntax)
      (let ((result (read-from-string "{:numbers [1 2.5 3/4], :strs [\"a\" \"b\"], :syms [foo bar], :bools [true false nil], :extra 42}")))
        (is (= (fset:lookup (fset:lookup result :numbers) 0) 1))
        (is (string= (fset:lookup (fset:lookup result :strs) 0) "a"))
        (is (eq (fset:lookup (fset:lookup result :syms) 0) 'foo))
        (is (eq (fset:lookup (fset:lookup result :bools) 0) t))
        (is (null (fset:lookup (fset:lookup result :bools) 2)))
        (is (= (fset:@ result :extra) 42))))

(test invalid-syntax-test
      (cave.reader:enable-clojure-syntax)
      (signals error (read-from-string "{:a 1 :b}"))  ;; Odd number of map elements
      (signals error (read-from-string "[1 2"))       ;; Unbalanced bracket
      (signals error (read-from-string "#{1 2"))       ;; Unbalanced set
)
