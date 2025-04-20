(ns backtick
  (:refer-clojure :exclude [resolve]))

(def ^:dynamic *resolve*)

(def ^:dynamic ^:private *gensyms*)

(defn- resolve [sym]
  (let [ns (namespace sym)
        n (name sym)]
    (if (and (not ns) (= (last n) \#))
      (if-let [gs (@*gensyms* sym)]
        gs
        (let [gs (gensym (str (subs n 0 (dec (count n))) "__auto__"))]
          (swap! *gensyms* assoc sym gs)
          gs))
      (*resolve* sym))))

(defn unquote? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote)))

(defn unquote-splicing? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote-splicing)))

(defn- -concat [parts]
  (case (count parts)
    0 nil
    1 (first parts)
    `(concat ~@parts)))

(defn- quote-fn* [form]
  (cond
    (symbol? form) `'~(resolve form)
    (unquote? form) (second form)
    (unquote-splicing? form) (throw (Exception. "splice not in list"))
    (record? form) `'~form
    (coll? form)
      (let [xs (if (map? form) (apply concat form) (seq form))
            splice-at (mapv unquote-splicing? xs)
            splice? (boolean (some identity splice-at))
            parts (mapv (fn [x]
                          (if (unquote-splicing? x)
                            (second x)
                            [(quote-fn* x)]))
                        xs)
            cat (-concat parts)]
        (cond
          (vector? form) (if splice?
                           `(vec ~cat)
                           (mapv first parts))
          (map? form) (if splice? 
                        `(apply hash-map ~cat)
                        (if (or (= 1 (count form))
                                (every? (some-fn keyword? number? char? string?) (keys form)))
                          (apply array-map (apply concat parts))
                          `(hash-map ~@(map first parts))))
          (set? form) (if splice?
                        `(set ~cat)
                        (case (count parts)
                          0 #{}
                          1 #{(ffirst parts)}
                          `(hash-set ~@(map first parts))))
          (seq? form) (if splice?
                        (let [first-splice (some #(when (nth splice-at %) %) (range (count splice-at)))]
                          `(apply list
                                  ~@(map first (subvec parts 0 first-splice))
                                  ~(if (= (inc first-splice) (count splice-at))
                                     (peek parts)
                                     (-concat (subvec parts first-splice)))))
                        (if (empty? parts)
                          ()
                          `(list ~@(map first parts))))
          :else (throw (Exception. "Unknown collection type"))))
    :else (if (or (keyword? form)
                  (number? form)
                  (char? form)
                  (string? form))
            form
            `'~form)))

(defn quote-fn [resolver form]
  (binding [*resolve* resolver
            *gensyms* (atom {})]
    (quote-fn* form)))

(defmacro defquote [name resolver]
  `(let [resolver# ~resolver]
     (defn ~(symbol (str name "-fn")) [form#]
       (quote-fn resolver# form#))
     (defmacro ~name [form#]
       (quote-fn resolver# form#))))

(defquote template identity)

(defn- class-symbol [^java.lang.Class cls]
  (symbol (.getName cls)))

(defn- namespace-name [^clojure.lang.Namespace ns]
  (name (.getName ns)))

(defn- var-namespace [^clojure.lang.Var v]
  (name (.name (.ns v))))

(defn- var-name [^clojure.lang.Var v]
  (name (.sym v)))

(defn- var-symbol [^clojure.lang.Var v]
  (symbol (var-namespace v) (var-name v)))

(defn- ns-resolve-sym [sym]
  (try
    (let [x (ns-resolve *ns* sym)]
      (cond
        (instance? java.lang.Class x) (class-symbol x)
        (instance? clojure.lang.Var x) (var-symbol x)
        ;; Workaround for change in 1.10
        (re-find #"\." (name sym)) sym
        :else nil))
    (catch ClassNotFoundException _
      sym)))

(defn resolve-symbol [sym]
  (let [ns (namespace sym)
        nm (name sym)]
    (if (nil? ns)
      (if-let [[_ ctor-name] (re-find #"(.+)\.$" nm)]
        (symbol nil (-> (symbol nil ctor-name)
                      resolve-symbol
                      name
                      (str ".")))
        (if (or (special-symbol? sym)
                (re-find #"^\." nm)) ; method name
          sym
          (or (ns-resolve-sym sym)
              (symbol (namespace-name *ns*) nm))))
      (or (ns-resolve-sym sym) sym))))

(defquote syntax-quote resolve-symbol)
