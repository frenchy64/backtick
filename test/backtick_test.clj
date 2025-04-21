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
    (is (contains?
          #{'(clojure.core/set (clojure.core/concat a b))
            '(clojure.core/set (clojure.core/concat b a))}
          (macroexpand-1 '(backtick/syntax-quote #{~@a ~@b}))))
    (is (= () (macroexpand-1 '(backtick/syntax-quote ()))))
    (is (= '(clojure.core/list 1 local-variable)
           (macroexpand-1 '(backtick/syntax-quote (1 ~local-variable)))))
    (is (= '(clojure.core/list* 1 (clojure.core/concat local-variable [2]))
           (macroexpand-1 '(backtick/syntax-quote (1 ~@local-variable 2)))))
    (is (= '(clojure.core/list* 1 local-variable)
           (macroexpand-1 '(backtick/syntax-quote (1 ~@local-variable)))))
    (is (= '(clojure.core/list* local-variable)
           (macroexpand-1 '(backtick/syntax-quote (~@local-variable)))))
    (is (= {} (macroexpand-1 '(backtick/syntax-quote {}))))
    (is (= '{local-variable1 local-variable2}
           (macroexpand-1 '(backtick/syntax-quote {~local-variable1 ~local-variable2}))))
    (is (= '{:a local-variable2 :b local-variable4}
           (macroexpand-1 '(backtick/syntax-quote {:a ~local-variable2 :b ~local-variable4}))))
    (is (= '(clojure.core/hash-map :a local-variable2 'backtick-test/a local-variable4)
           (macroexpand-1 '(backtick/syntax-quote {:a ~local-variable2 a ~local-variable4}))))
    (is (= '(clojure.core/hash-map local-variable1 local-variable2 local-variable3 local-variable4)
           (macroexpand-1 '(backtick/syntax-quote {~local-variable1 ~local-variable2 ~local-variable3 ~local-variable4}))))
    (is (= '(clojure.core/apply clojure.core/hash-map (clojure.core/concat local-variable1 local-variable2))
           (macroexpand-1 '(backtick/syntax-quote {~@local-variable1 ~@local-variable2}))))
    (is (= 42 (macroexpand-1 '(backtick/syntax-quote 42))))
    (is (= :a (macroexpand-1 '(backtick/syntax-quote :a))))
    (is (= \a (macroexpand-1 '(backtick/syntax-quote \a))))
    (is (= "a" (macroexpand-1 '(backtick/syntax-quote "a"))))
    (is (nil? (macroexpand-1 '(backtick/syntax-quote nil))))
    (is (= "#\"a\"" (pr-str (macroexpand-1 '(backtick/syntax-quote #"a")))))
    ;;FIXME should be (quote #"a")
    (is (= "(clojure.core/list (quote quote) #\"a\")" (pr-str (macroexpand-1 '(backtick/syntax-quote '#"a")))))
    ;;FIXME should be (clojure.core/list (quote clojure.core/let) [foo 42] (quote (clojure.core/+ user/foo user/foo)))
    (is (= '(clojure.core/list 'clojure.core/let [foo 42] (clojure.core/list 'clojure.core/+ 'backtick-test/foo 'backtick-test/foo))
           (macroexpand-1 '(backtick/syntax-quote (let [~foo 42] (+ foo foo))))))
    ;;FIXME should be (quote (nil))
    (is (= '(clojure.core/list nil)
           (macroexpand-1 '(backtick/syntax-quote (nil)))))
    (is (= (list 'quote 'foo) (macroexpand-1 '(backtick/syntax-quote ~'foo))))
    ;;FIXME should be (quote (foo foo))
    (is (= '(clojure.core/list 'foo 'foo)
           (macroexpand-1 '(backtick/syntax-quote (~'foo ~'foo)))))
    ))

(defn unreasonably-quick-benchmark* [f {:keys [stop-fn] :as opts}]
  (let [start (. System (nanoTime))
        stop-fn (or stop-fn #(assert (not (Thread/interrupted))))
        samples (:samples opts 100)
        _ (dotimes [_ samples]
            (stop-fn)
            (f))
        end (. System (nanoTime))]
    (assert (pos? samples))
    {:mean (double (/ (- end start) samples))}))

(comment
  (unreasonably-quick-benchmark* #(eval nil) {:samples 100000})
  (unreasonably-quick-benchmark* #(eval (macroexpand-1 '(syntax-quote nil))) {:samples 100})
  (unreasonably-quick-benchmark* #(eval '`nil) {:samples 100})
  (unreasonably-quick-benchmark* #(eval '`(apply + 1 2 3)) {:samples 100000})
  (unreasonably-quick-benchmark* #(eval '`(apply + 1 2 3)) {:samples 1000})
  (unreasonably-quick-benchmark* #(eval (macroexpand-1 '(syntax-quote (apply + 1 2 3)))) {:samples 1000})
)

(defn bench-eval-expanded-syntax-quote [input {:keys [benchmark* stop-fn] :or {benchmark* unreasonably-quick-benchmark*}
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
        backtick-mean (:mean backtick-bench)
        multiplier (double (/ backtick-mean clojure-mean))]
    ;(println)
    ;(println (str "Evaluating the expansion of (backtick/syntax-quote " (pr-str input) ") takes "
    ;              multiplier
    ;              " of the execution time of `" (pr-str input)))
    ;(println "- backtick expansion:" (pr-str expanded-backtick-syntax-quoted))
    ;(println "- Clojure expansion:" (pr-str expanded-clojure-syntax-quote))
    multiplier))

(def bench-cases
  [42    ;; backtick is ~1x clojure
   :foo  ;; backtick is ~1x clojure
   "a"   ;; backtick is ~1x clojure
   nil   ;; backtick is ~1x clojure (FIXME often is 0.9x! why??)
   []    ;; backtick is ~0.35x clojure
   {}    ;; backtick is ~0.32x clojure
   ()    ;; backtick is ~0.47x clojure
   [[[[]]]] ;; backtick is ~0.32x clojure
   '(let [~(identity 'foo) 42] (+ foo foo)) ;; backtick is ~0.7x clojure
   '(binding [] ~@(identity []))  ;; backtick is ~0.67x clojure
   {:foo 42} ;; backtick is ~0.37x clojure
   {:foo 42 :bar 24 :baz 128} ;; backtick is ~0.29x clojure
   #{:foo 42 :bar 24 :baz 128} ;; backtick is ~0.65x clojure
   ;; backtick is ~0.75x clojure (from clojure.core/destructure)
   '(if (seq? ~(identity 'gmap))
      (if (next ~(identity 'gmapseq))
        (clojure.lang.PersistentArrayMap/createAsIfByAssoc (to-array ~(identity 'gmapseq)))
        (if (seq ~(identity 'gmapseq)) (first ~(identity 'gmapseq)) clojure.lang.PersistentArrayMap/EMPTY))
      ~(identity 'gmap))
   ;; backtick is ~0.75x clojure (from clojure.core/destructure)
   '(fn ~(identity 'giter) [~(identity 'gxs)]
      (lazy-seq
        (loop [~(identity 'gxs) ~(identity 'gxs)]
          (when-let [~(identity 'gxs) (seq ~(identity 'gxs))]
            (if (chunked-seq? ~(identity 'gxs))
              (let [~(identity 'c) (chunk-first ~(identity 'gxs))
                    ~(identity 'size) (int (count ~(identity 'c)))
                    ~(identity 'gb) (chunk-buffer ~(identity 'size))]
                (if (loop [~(identity 'gi) (int 0)]
                      (if (< ~(identity 'gi) ~(identity 'size))
                        (let [~(identity 'bind) (.nth ~(identity 'c) ~(identity 'gi))]
                          ~(identity '(do-cmod mod-pairs)))
                        true))
                  (chunk-cons
                    (chunk ~(identity 'gb))
                    (~(identity 'giter) (chunk-rest ~(identity 'gxs))))
                  (chunk-cons (chunk ~(identity 'gb)) nil)))
              (let [~(identity 'bind) (first ~(identity 'gxs))]
                ~(identity '(do-mod mod-pairs))))))))
   ])

#_
(deftest bench
  (binding [*ns* (the-ns 'backtick-test)]
    (let [testing-thread (Thread/currentThread)
          stop-fn #(assert (not (.isInterrupted testing-thread)))]
      (doto (mapv
              (fn [c]
                (let [bench1 (fn [attempt]
                               ;(println "Attempt" attempt "for" (pr-str c))
                               (let [_ (stop-fn)
                                     ;; blindly remove this many "outliers" from each end, just assume the mean is the middle
                                     remove-outliers 3
                                     times 10
                                     _ (assert (< (* 2 remove-outliers) times))
                                     multipliers (mapv
                                                   (fn [i]
                                                     (stop-fn)
                                                     ;(println "Iteration" i)
                                                     (bench-eval-expanded-syntax-quote
                                                       c
                                                       {:stop-fn stop-fn
                                                        :benchmark* unreasonably-quick-benchmark*
                                                        :samples 100}))
                                                   (range times))
                                     multipliers (vec (sort multipliers))
                                     multipliers (subvec multipliers remove-outliers (- times remove-outliers))
                                     avg (double (/ (apply + multipliers) (count multipliers)))]
                                 (println)
                                 (println (str (format "[%s] " avg)
                                               "After " times " iterations, the average time "
                                               "it takes to evaluate the expansion of "
                                               (pr-str (list 'syntax-quote c))
                                               " in backtick is " avg " of the execution time of `" (pr-str c)))
                                 avg))]
                  [c (mapv bench1 (range 5))]))
              bench-cases)
        prn)))
)

(comment
  (bench/quick-bench (eval `(apply list [])) :verbose)
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
  (do '`~`:a)
  (macroexpand-1 '(backtick/syntax-quote ~(macroexpand-1 '(backtick/syntax-quote :a))))
  (do '`nil)
  (do (macroexpand-1 '(backtick/syntax-quote nil)))
  )

