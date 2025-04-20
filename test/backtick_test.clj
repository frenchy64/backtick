(ns backtick-test
  (:use clojure.test)
  (:require [backtick :refer (template defquote quote-fn syntax-quote)]
            [criterium.core :as bench]))

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
    (is (= '[1 a] (macroexpand-1 '(backtick/syntax-quote [1 ~a]))))
    (is (= '[local-variable] (macroexpand-1 '(backtick/syntax-quote [~local-variable]))))
    (is (= '(clojure.core/vec local-variable)
           (macroexpand-1 '(backtick/syntax-quote [~@local-variable]))))
    (is (= #{} (macroexpand-1 '(backtick/syntax-quote #{}))))
    (is (= '#{a} (macroexpand-1 '(backtick/syntax-quote #{~a}))))
    (is (= '(clojure.core/set a)
           (macroexpand-1 '(backtick/syntax-quote #{~@a}))))
    (is (= '(clojure.core/hash-set a b)
           (macroexpand-1 (list 'backtick/syntax-quote (sorted-set-by #(compare (last %1) (last %2)) '~a '~b)))))
    ;; TODO more direct, but introducing dependency on `into`: (into (set a) b)
    (is (contains?
          #{'(clojure.core/set (clojure.core/concat a b))
            '(clojure.core/set (clojure.core/concat b a))}
          (macroexpand-1 '(backtick/syntax-quote #{~@a ~@b}))))
    (is (= () (macroexpand-1 '(backtick/syntax-quote ()))))
    (is (= '(clojure.core/list 1 local-variable)
           (macroexpand-1 '(backtick/syntax-quote (1 ~local-variable)))))
    (is (= '(clojure.core/apply clojure.core/list 1 (clojure.core/concat local-variable [2]))
           (macroexpand-1 '(backtick/syntax-quote (1 ~@local-variable 2)))))
    (is (= '(clojure.core/apply clojure.core/list 1 local-variable)
           (macroexpand-1 '(backtick/syntax-quote (1 ~@local-variable)))))
    (is (= '(clojure.core/apply clojure.core/list local-variable)
           (macroexpand-1 '(backtick/syntax-quote (~@local-variable)))))
    (is (= {} (macroexpand-1 '(backtick/syntax-quote {}))))
    (is (= '{local-variable1 local-variable2}
           (macroexpand-1 '(backtick/syntax-quote {~local-variable1 ~local-variable2}))))
    (is (= '(clojure.core/hash-map local-variable1 local-variable2 local-variable3 local-variable4)
           (macroexpand-1 '(backtick/syntax-quote {~local-variable1 ~local-variable2 ~local-variable3 ~local-variable4}))))
    (is (= '(clojure.core/apply clojure.core/hash-map (clojure.core/concat local-variable1 local-variable2))
           (macroexpand-1 '(backtick/syntax-quote {~@local-variable1 ~@local-variable2}))))
    (is (= 42 (macroexpand-1 '(backtick/syntax-quote 42))))
    (is (= :a (macroexpand-1 '(backtick/syntax-quote :a))))
    (is (= \a (macroexpand-1 '(backtick/syntax-quote \a))))
    (is (= "a" (macroexpand-1 '(backtick/syntax-quote "a"))))
    (is (= "(quote #\"a\")" (pr-str (macroexpand-1 '(backtick/syntax-quote #"a")))))))

(defn unreasonably-quick-benchmark* [f opts]
  (let [start (. System (nanoTime))
        samples (:samples opts 100)
        _ (dotimes [_ samples] (f))
        end (. System (nanoTime))]
    (assert (pos? samples))
    {:mean (/ (- end start) samples)}))

(comment
  (unreasonably-quick-benchmark* #(eval nil) nil)
)

(defn bench-eval-expanded-syntax-quote [input {:keys [benchmark*] :or {benchmark* bench/quick-benchmark*}
                                               :as opts}]
  (let [expanded-clojure-syntax-quote (read-string (str "`" (pr-str input)))
        expanded-backtick-syntax-quoted (macroexpand-1 (list `backtick/syntax-quote input))
        clojure-result (eval expanded-clojure-syntax-quote)
        backtick-result (eval expanded-backtick-syntax-quoted)
        ;;TODO check collection types align, account for auto gensym
        _ (when-not (= clojure-result backtick-result)
            (throw (ex-info "Form evaluates to different results"
                            {:input input
                             :clojure-result clojure-result
                             :backtick-result backtick-result})))
        ;;TODO check all the results align
        clojure-bench (benchmark* #(clojure.lang.Compiler/eval expanded-clojure-syntax-quote) opts)
        backtick-bench (benchmark* #(clojure.lang.Compiler/eval expanded-backtick-syntax-quoted) opts)
        clojure-mean (:mean clojure-bench)
        backtick-mean (:mean backtick-bench)]
    (println)
    (println (str "Evaluating the expansion of (backtick/syntax-quote " (pr-str input) ") takes "
                  (double (/ backtick-mean clojure-mean))
                  " of the execution time of `" (pr-str input)))
    (println "- backtick expansion:" (pr-str expanded-backtick-syntax-quoted))
    (println "- Clojure expansion:" (pr-str expanded-clojure-syntax-quote))
    ))

(def bench-cases
  [nil
   42
   []
   {}
   ()
   '(let [~'foo 42] (+ foo foo))
   '(binding [] ~@[])])

#_
(deftest bench
  (binding [*ns* (the-ns 'backtick-test)]
    (doseq [c bench-cases]
      (bench-eval-expanded-syntax-quote
        c
        {:benchmark* unreasonably-quick-benchmark*
         :samples 10000})))
)

(comment
  (do '`42)
  (macroexpand-1 '(backtick/syntax-quote 42))
  (do '`:foo)
  (macroexpand-1 '(backtick/syntax-quote :foo))
  (do '`foo)
  (macroexpand-1 '(backtick/syntax-quote foo))
  (eval (do '`(:foo)))
  (eval (macroexpand-1 '(backtick/syntax-quote (:foo))))
  (eval (do '`[:foo]))
  (eval (macroexpand-1 '(backtick/syntax-quote [:foo])))
  (eval (do '`[foo]))
  (eval (macroexpand-1 '(backtick/syntax-quote [foo])))
  (eval (do '`'(42)))
  (eval (macroexpand-1 '(backtick/syntax-quote '(42))))
  (eval (do '`''nil))
  (eval (macroexpand-1 '(backtick/syntax-quote ''nil)))

  (do '`:a)
  (do '`a)
  (do ```:a)
  (do ```42)
  (eval (list 'backtick/syntax-quote
              (macroexpand-1 (list 'backtick/syntax-quote
                                   (macroexpand-1 (list 'backtick/syntax-quote 42))))))
  )
