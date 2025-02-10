(in-package #:cave/test)

(def-suite reader-suite
           :description "Tests for Clojure-style reader macros"
           :in cave-suite)
(in-suite reader-suite)

#.(cave.reader:enable-clojure-syntax)

(test vector-reader-test
      (let ((result [1 2 3]))
        (is (typep result 'fset:seq))
        (is (= 3 (fset:size result)))
        (is (equal '(1 2 3)
                  (fset:convert 'list result)))))

(test map-reader-test
      (cave.reader:enable-clojure-syntax)
      (let ((result {:a 1 :b 2}))
        (is (typep result 'fset:map))
        (is (= 2 (fset:size result)))
        (is (= 1 (fset:@ result :a)))
        (is (= 2 (fset:@ result :b)))))

(test set-reader-test
      (cave.reader:enable-clojure-syntax)
      (let ((result #{1 2 3}))
        (is (typep result 'fset:set))
        (is (= 3 (fset:size result)))
        (is (fset:equal? result (fset:set 1 2 3))))
      ;; Test empty set
      (let ((empty-set #{}))
        (is (zerop (fset:size empty-set)))))

(test nested-structures-test
      (cave.reader:enable-clojure-syntax)
      (let ((result [{:a [1 2] :b #{3}} :foo {:x 'y}]))
        (is (typep (fset:lookup result 0) 'fset:map))
        (is (typep (fset:lookup (fset:lookup result 0) :a) 'fset:seq))
        (is (typep (fset:lookup (fset:lookup result 0) :b) 'fset:set))
        (is (fset:contains? (fset:lookup (fset:lookup result 0) :b) 3))
        (is (typep (fset:lookup result 2) 'fset:map))
        (is (eq (fset:lookup result 1) :foo)))
      ;; Test empty nested collections
      (let ((result2 [{} [] #{}]))
        (is (typep (fset:lookup result2 0) 'fset:map))
        (is (typep (fset:lookup result2 1) 'fset:seq))
        (is (typep (fset:lookup result2 2) 'fset:set))
        (is (zerop (fset:size (fset:lookup result2 0))))
        (is (zerop (fset:size (fset:lookup result2 1))))
        (is (zerop (fset:size (fset:lookup result2 2))))))

(test complex-map-keys-test
      (cave.reader:enable-clojure-syntax)
      ;; Test using vectors as map keys
      (let ((result { [1 2] :vec-key, {:a 1} :map-key }))
        (is (eq :vec-key (fset:@ result [1 2])))
        (is (eq :map-key (fset:@ result {:a 1})))
        ;; Test nested complex keys
        (let ((nested { [{:x 1} [2 3]] :nested }))
          (is (eq :nested (fset:@ nested [{:x 1} [2 3]]))))))

(test mixed-data-types-test
      (cave.reader:enable-clojure-syntax)
      (let ((result {:numbers [1 2.5 3/4], :strs ["a" "b"], :syms [foo bar], :bools [true false nil], :extra 42}))
        (is (= (fset:lookup (fset:lookup result :numbers) 0) 1))
        (is (string= (fset:lookup (fset:lookup result :strs) 0) "a"))
        (is (eq (fset:lookup (fset:lookup result :syms) 0) 'foo))
        (is (eq (fset:lookup (fset:lookup result :bools) 0) t))
        (is (null (fset:lookup (fset:lookup result :bools) 2)))
        (is (= (fset:@ result :extra) 42))))

(test comment-syntax-test
  (cave.reader:enable-clojure-syntax)
  ;; Test skipping multiple forms using parentheses
  (is (equalp (fset:map ("a" 1) ("b" 2))
              {#_ (:commented-key 99) "a" 1 "b" 2}))
  ;; Alternative: Skip both key and value separately
  (is (equalp (fset:map ("b" 2))
              {#_ :key #_ 99 "b" 2})))

(test arbitrary-map-keys-test
  (cave.reader:enable-clojure-syntax)
  ;; Use unquoted symbols for map keys/values
  (let ((result {"string" 1, 42 answer, sym val, TRUE :true-value}))
    (is (= 1 (fset:@ result "string")))
    (is (eq 'answer (fset:@ result 42)))
    (is (eq 'val (fset:@ result 'sym)))
    (is (eq :true-value (fset:@ result t)))
    ;; Test non-hashable key error
    (signals error (read-from-string "{(lambda (x) x) 42}"))))

(test invalid-syntax-test
      (cave.reader:enable-clojure-syntax)
      (signals error (read-from-string ""))  ;; Odd number of map elements
      (signals error (read-from-string "[1 2"))       ;; Unbalanced bracket
      (signals error (read-from-string "#{1 2"))       ;; Unbalanced set
)
