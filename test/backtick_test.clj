(ns backtick-test
  (:use clojure.test)
  (:require [backtick :refer (template defquote quote-fn syntax-quote)]))

(deftest template-test

  (testing "Primitives, collections, unquote, and splice; symbols qualified"
    (let [n 5 v [:a :b]]
      (is (=           `(5 nil () true a/b ~n [p/q ~@v r/s] {:x #{"s"}})
              (template (5 nil () true a/b ~n [p/q ~@v r/s] {:x #{"s"}}))))))

  (testing "Multiple splices"
    (let [v [:a :b] a 5]
      (is (=           `(~a ~@v ~@v ~a)
             (template  (~a ~@v ~@v ~a))))))

  (testing "Automatic gensyms"
    (let [[a b c d] (template [foo# bar# foo# bar])]
      (is (not= a 'foo))
      (is (not= a 'foo#))
      (is (= a c))
      (is (not= a b))
      (is (= d 'bar)))))

(defn add-bang [sym]
  (symbol (str sym "!")))

(defquote wacky-quote add-bang)

(deftest defquote-test
  (testing "Custom resolver"
    (is (= ''foo! (quote-fn add-bang 'foo)))
    (is (= ''foo! (wacky-quote-fn 'foo)))
    (is (= '(foo! :a [5 bar!])
           (wacky-quote (foo :a [5 bar]))))))

(defrecord R [x])

(deftest record-test
  (testing "Record types are preserved"
    (is (= (R. 1) (template #backtick_test.R{:x 1})))))

(deftest syntax-quote-test
  (testing "Constructors, classes, methods, vars, and specials"
    (is (= (syntax-quote
             [Class
              Class.
              java.lang.Class
              java.lang.Class.
              unqualified
              fully/qualified
              .method
              .
              non.existant
              inc
              backtick.test/inc
              quote])
           `[Class
             Class.
             java.lang.Class
             java.lang.Class.
             unqualified
             fully/qualified
             .method
             .
             non.existant
             inc
             backtick.test/inc
             quote]))))

;;TODO fuzz test correspondence between ` and syntax-quote
(deftest expansion-tests
  (binding [*ns* (the-ns 'backtick-test)]
    (is (= [] (macroexpand-1 '(backtick/syntax-quote []))))
    (is (= '['1 a] (macroexpand-1 '(backtick/syntax-quote [1 ~a]))))
    (is (= '[local-variable] (macroexpand-1 '(backtick/syntax-quote [~local-variable]))))
    ;; OK (could remove concat call for bonus points)
    (is (= '(clojure.core/vec (clojure.core/concat local-variable))
           (macroexpand-1 '(backtick/syntax-quote [~@local-variable]))))
    (is (= #{} (macroexpand-1 '(backtick/syntax-quote #{}))))
    (is (= '#{a} (macroexpand-1 '(backtick/syntax-quote #{~a}))))
    (is (= '(clojure.core/set a)
           (macroexpand-1 '(backtick/syntax-quote #{~@a}))))
    (is (= '(clojure.core/hash-set a b)
           (macroexpand-1 (list 'backtick/syntax-quote (sorted-set-by #(compare (last %1) (last %2)) '~a '~b)))))
    ;;OK
    (is (= '(clojure.core/set (clojure.core/concat a b))
           (macroexpand-1 (list 'backtick/syntax-quote (sorted-set-by #(compare (last %1) (last %2)) '~@a '~@b)))))
    (is (= '(clojure.core/list '1 local-variable)
           (macroexpand-1 '(backtick/syntax-quote (1 ~local-variable)))))
    (is (= '(clojure.core/apply clojure.core/list '1 (clojure.core/concat local-variable ['2]))
           (macroexpand-1 '(backtick/syntax-quote (1 ~@local-variable 2)))))
    (is (= '(clojure.core/apply clojure.core/list '1 local-variable)
           (macroexpand-1 '(backtick/syntax-quote (1 ~@local-variable)))))
    (is (= '(clojure.core/apply clojure.core/list local-variable)
           (macroexpand-1 '(backtick/syntax-quote (~@local-variable)))))
    ;;TODO should be {}
    (is (= '(clojure.core/apply clojure.core/hash-map (clojure.core/concat))
           (macroexpand-1 '(backtick/syntax-quote {}))))
    ;;TODO should be (hash-map local-variable1 local-variable2)
    (is (= '(clojure.core/apply clojure.core/hash-map (clojure.core/concat [local-variable1] [local-variable2]))
           (macroexpand-1 '(backtick/syntax-quote {~local-variable1 ~local-variable2}))))
    ;; OK
    (is (= '(clojure.core/apply clojure.core/hash-map (clojure.core/concat local-variable1 local-variable2))
           (macroexpand-1 '(backtick/syntax-quote {~@local-variable1 ~@local-variable2}))))))
