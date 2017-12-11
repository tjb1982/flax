(ns co.nclk.flax.core
  (:require [clj-yaml.core :as yaml]
            [clojure.tools.logging :refer [log]]
            [co.nclk.flax.data :as data]
            [cheshire.core :as json]
            [cheshire.generate :as cheshire]
            [stencil.parser :refer [parse]]
            [stencil.core :refer [render]])
  ;; FIXME: does this belong here? Should there be a generic mechanism for 
  ;; importing other modules from various places? At the moment, I think it
  ;; sounds like a good idea to have this be a protocol with a single function
  ;; `resolve` and then let clients implement their own. Then we'd have to
  ;; provide some way of injecting it (rather than importing it here).
  (:import co.nclk.flax.data.FileDataConnector)
  (:gen-class))

(cheshire/add-encoder clojure.lang.Var cheshire/encode-str)

(def parser-options (atom {:tag-open "~{" :tag-close "}"}))
(def genv (atom
            (into {}
              (for [[k v] (System/getenv)]
                [(keyword k) v]))))


(defn upmap
  [fn coll & [delay]]
  (doall
    (for [p (doall
              (map
                #(do
                   (Thread/sleep (or delay 0))
                   (future (fn %)))
                coll))]
      @p)))


(defn get-with-string-maybe-index
  [key* env]
  (if (sequential? env)
    (let [idx (Integer/parseInt key*)]
      (nth env idx))
    (let [k (keyword key*)]
      (get env k))))

(defn dot-get
  [target env]
  (loop [target target env env]
    (cond
      (-> target (.contains "."))
      (let [parts (-> target (clojure.string/split #"\." 2))]
        ;; TODO support bracket sugar (e.g., foo[0].bar[baz])
        (recur (second parts)
               (get-with-string-maybe-index (first parts) env)))
      :else
      (when-not (or (not (coll? env))
                    (empty? env)
                    (clojure.string/blank? target))
        (let [data (get-with-string-maybe-index target env)]
          (if (string? data) (clojure.string/trim data) data))))))

(defn pprint-interpolate
  [s label handler env]
  (let [regex (re-pattern (str "~\\{" (name label) ":[\\w\\.-]+\\}"))]
    (loop [s s]
      (let [match (re-find regex s)]
        (if (nil? match)
          s
          (let [v (-> match
                      (subs (+ 3 (-> label name count))
                            (- (count match) 1))
                      (dot-get env))]
            (recur (clojure.string/replace-first
                     s
                     regex
                     (handler v)))))))))

(defn eval-clj-string
  [s env evaluate]
  (let [form (clojure.walk/postwalk
               (fn [x]
                 (if (map? x)
                   (into {} x)
                   x))
               (-> s (evaluate env) read-string))]
    (eval form)))

(defn swap
  [s env & [evaluate]]
  (cond

    ;; Env dump.
    (= (clojure.string/trim s) "~@")
    env

    ;; Support for keyword literals.
    (.startsWith s "~:")
    (keyword (subs s 2))

    ;; Support for symbol literals.
    (.startsWith s "~'")
    (symbol (subs s 2))

    ;; Support for resolved symbol literals.
    (.startsWith s "~>'")
    (resolve (symbol (subs s 3)))

    ;; If `s` starts with ~@, then replace it with the data held by the
    ;; variable (i.e., not a string interpolation of it).
    ;; Supports '.' notation (e.g., foo.bar.0.baz etc.)
    (.startsWith s "~@")
    (let [target (-> s (subs 2))]
      (dot-get target env))

    ;; If `s` starts with "~$", then replace it with the
    ;; stdout result of running the command locally.
    (.startsWith s "~$")
    (let [s (swap (subs s 2) env evaluate)
          proc (-> (Runtime/getRuntime)
                 (.exec (into-array String
                          ["bash" "-c" (clojure.string/trim s)])))
          stdout (clojure.java.io/reader (.getInputStream proc))]
      (clojure.string/join "\n"
        (loop [lines []]
          (let [line (.readLine stdout)]
            (if (nil? line)
              lines
              (recur (conj lines line)))))))

    (and (fn? evaluate) (.startsWith s "~clj"))
    (eval-clj-string (subs s 4) env evaluate)

    ;; Interpolate.
    :else (-> s
            (pprint-interpolate :json json/generate-string env)
            (pprint-interpolate :yaml yaml/generate-string env)
            (pprint-interpolate :pprint
                                #(-> % clojure.pprint/pprint
                                       with-out-str
                                       clojure.string/trim)
                                env)
            (parse @parser-options)
            (render env)
            )))

(defn evaluate-function-or-special-form
  [m env config evaluate]
  (let [fun-entry (->> m
                       (filter
                         #(-> % key str (subs 1) (.startsWith "~(")))
                       first)
        fun (-> fun-entry key str (subs 3) symbol)
        do-statements (fn [statements config]
                        (loop [statements statements last-ret nil]
                          (if (empty? statements)
                            last-ret
                            (let [ret (evaluate (first statements) config)]
                              (recur (drop 1 statements) ret)))))]
    ;; XXX: An interesting idea because it's more "pure", but not sure it works.
    ;;(let [sexp (conj
    ;;             (->> (val fun-entry)
    ;;                  (map-indexed
    ;;                    #(let [item (evaluate %2 config)]
    ;;                      (if (contains? #{'let 'fn 'binding 'for} fun)
    ;;                        (if (= %1 0)
    ;;                          (into [] item) item)
    ;;                        (if (contains? #{'defn} fun)
    ;;                          (if (= %1 1)
    ;;                            (into [] item) item)
    ;;                          item)))))
    ;;             fun)]
    ;;  (eval sexp))

    (condp = fun
      ;; Special forms

      'clj
      (eval-clj-string (-> fun-entry val first) env evaluate)

      'if
      (evaluate
        ((if (evaluate (-> fun-entry val first) config)
           second #(nth % 2 nil))
          (-> fun-entry val))
        config)

      'let
      (let [bindings (-> fun-entry val first)]
        (if-not (even? (count bindings))
          (throw (IllegalArgumentException.
                   (format
                     "`let` requires an even number of bindings: %s"
                     bindings)))
          (let [bindings (-> fun-entry val first)
                statements (->> fun-entry val (drop 1))
                env (loop [bindings bindings
                           env env]
                      (if (empty? bindings)
                        env
                        (recur (drop 2 bindings)
                               (merge env
                                      {(evaluate
                                         (keyword 
                                           (first bindings))
                                         (assoc config :env env))
                                       (evaluate
                                         (second bindings)
                                         (assoc config :env env))}))))]
            (do-statements statements (assoc config :env env)))))

      'fn
      (fn [& argv]
        (let [args (-> fun-entry val first) ;; list of strings
              statements (->> fun-entry val (drop 1))
              env (loop [args args
                         argv argv
                         env env]
                    (if (empty? args)
                      env
                      (let [env (merge env
                                       {(evaluate
                                          (keyword (first args)) config)
                                        (evaluate (first argv) config)})]
                        (recur (drop 1 args)
                               (drop 1 argv)
                               env))))]
          (do-statements statements (assoc config :env env))))

      (symbol "#")
      (fn [& argv]
        (let [env (merge env
                         (into {}
                           (map-indexed
                             (fn [idx item]
                               [(keyword (str idx))
                                (evaluate item config)])
                             argv)))]
          (do-statements (-> fun-entry val) (assoc config :env env))))

      'pathwise
      (let [parts (clojure.string/split
                    (-> fun-entry val (nth 2) (evaluate config))
                    #"\.")
            cfun (-> fun-entry val first (evaluate config))
            pred (-> fun-entry val second (evaluate config))
            testers (map #(dot-get % env)
                         (reduce
                           (fn [coll part]
                             (if (last coll)
                               (conj coll (str (last coll) "." part))
                               (conj coll part)))
                           []
                           parts))]
        (cfun pred testers))

      'for
      (let [coll (-> fun-entry val first second (evaluate config))]
        (doall
          (map
            #(let [env (merge env
                              {(keyword
                                 (-> fun-entry
                                     val
                                     ffirst
                                     (evaluate config))) %})]
              (evaluate
                (->> fun-entry val (drop 1)) config))
            coll)))

      'or
      (loop [args (-> fun-entry val)]
        (when-not (empty? args)
          (let [yield (evaluate (first args) config)]
            (if-not yield
              (recur (drop 1 args))
              yield))))

      'and
      (loop [args (-> fun-entry val)
             last-yield nil]
        (if (empty? args)
          last-yield
          (let [yield (evaluate (first args) config)]
            (if-not yield
              false
              (recur (drop 1 args) yield)))))

      'parallel
      (upmap #(evaluate % config)
        (-> fun-entry val))

      ;; FIXME: rename this to something more indicative of what it does.
      'upmap
      (apply upmap (evaluate (-> m first val) config))

      'log
      (let [args (-> fun-entry val)]
        (log (evaluate (-> args first keyword) config)
             (apply str
               (map #(with-out-str (clojure.pprint/pprint (evaluate % config)))
                    (drop 1 args)))))


      ;; Functions
      (let [yield (apply (resolve fun)
                         (evaluate (-> m first val) config))]
        (if (coll? yield)
          ;; FIXME: Laziness disabled?
          (doall yield)
          yield)))))

(defn evaluate-end-dotted-entry
  [k v oldval selvec env config evaluate]
  (let [ev (evaluate v (assoc config
                         :env
                         (merge env (get-in oldval selvec))))
        newval (reduce (fn [p k]
                         (assoc-in oldval
                           (conj selvec k)
                           (-> v (get (keyword k)))))
                       oldval
                       (keys v))]
    (println k selvec)
    {(-> k name (clojure.string/split #"\.") first keyword) (evaluate newval config)}
    ))

(defn parse-keyparts
  [k]
  (->> (clojure.string/split (name k) #"\." -1)
       (reduce
         (fn [p c]
           (cond
             (nil? (last p))
             (conj p c)

             (clojure.string/includes? (last p) "\\")
             (conj
               ;; `vec` because we need to conj to the end.
               (-> p count dec (take p) vec)
               (str
                 (subs (last p) 0 (-> p last count dec))
                 "." c))

             :else
             (conj p c)))
         [])
       (map #(try
              (Integer/parseInt %)
              (catch java.lang.NumberFormatException nfe
                (if (clojure.string/blank? %)
                  nil (keyword %)))))
       (into [])))

(defn evaluate-dotted-entry
  [m k v env]
  (let [ks (parse-keyparts k)]
    (assoc-in env (vec ks) v)))

    ;;((fn go [m [k & ks] v]
    ;;  (assoc-in env (vec (conj ks k)) v)
    ;;  #_(if ks
    ;;    (if (-> ks first nil?)
    ;;      (assoc m k ((if (sequential? v) concat merge) (get m k) v))
    ;;      (assoc m k (go (get m k) ks v)))
    ;;    (assoc m k v)))
    ;;  m keyparts v)
    ;;))
        

(defn evaluate-map-literal
  [m env config evaluate]
  (if (record? m)
    m
    (into (empty m)
      (reduce
        (fn [nm [k v]]
          (let [k (evaluate k config)]
            (conj nm
              (cond
                (= k :assert)
                [k v]

                (-> k name (.contains "."))
                (evaluate-dotted-entry nm k (evaluate v config) env)

                ;; k is already evaluated above
                :else [k (evaluate v config)]))))
        {}
        m))))

(defn evaluate-map
  [m env config evaluate]
  (cond
    ;; Functions and special forms
    (->> (keys m) (some #(-> % str (subs 1) (.startsWith "~("))))
    (evaluate-function-or-special-form m env config evaluate) 

    ;; All other maps
    :else
    (evaluate-map-literal m env config evaluate)))

(defn evaluate
  ;; XXX: what is `regex-str`? It doesn't seem to be used anywhere.
  [m & [config custom-eval regex-str]]
  (let [evaluate (or custom-eval evaluate)
        env (or (:env config) config)]
    (try
      (cond
        (string? m)
        (swap m env evaluate)

        (keyword? m)
        (keyword (swap (subs (str m) 1) env))

        (symbol? m)
        (symbol (swap (name m) env))

        (map? m)
        (evaluate-map m env config evaluate)

        ;; Other collections
        (coll? m)
        (into (if (seq? m) [] (empty m))
              (doall (map (fn [item] (evaluate item config)) m)))

        :else m)
    (catch Exception e
      (throw 
        (RuntimeException.
          (json/generate-string {:env env})
          e)))
      )))


(def ascii-art "
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»»»»‹‹‹‹‹‹‹‹‹‹‹‹‹»»»‹‹‹»»»»»»»»»»»»»»»‹‹‹‹‹‹‹‹‹»»»»»‹‹‹»»»»»»»‹‹‹»»»»»»»»»»»»~
~»»»»‹‹‹‹‹‹‹‹‹‹‹‹‹»»»‹‹‹»»»»»»»»»»»»»‹‹‹‹‹‹‹‹‹‹‹‹‹»»»‹‹‹»»»»»»»‹‹‹»»»»»»»»»»»»~
~»»»»‹‹‹»»»»»»»»»»»»»‹‹‹»»»»»»»»»»»»»‹‹‹»»»»»»»‹‹‹»»»‹‹‹»»»»»»»‹‹‹»»»»»»»»»»»»~
~»»»»‹‹‹‹‹‹‹‹‹‹‹‹‹»»»‹‹‹»»»»»»»»»»»»»‹‹‹‹‹‹‹‹‹‹‹‹‹»»»»»‹‹‹»»»‹‹‹»»»»»»»»»»»»»»~
~»»»»‹‹‹‹‹‹‹‹‹‹‹‹‹»»»‹‹‹»»»»»»»»»»»»»‹‹‹‹‹‹‹‹‹‹‹‹‹»»»»»»»‹‹‹‹‹»»»»»»»»»»»»»»»»~
~»»»»‹‹‹»»»»»»»»»»»»»‹‹‹»»»»»»»»»»»»»‹‹‹»»»»»»»‹‹‹»»»»»‹‹‹»»»‹‹‹»»»»»»»»»»»»»»~
~»»»»‹‹‹»»»»»»»»»»»»»‹‹‹»»»»»»»»»»»»»‹‹‹»»»»»»»‹‹‹»»»‹‹‹»»»»»»»‹‹‹»»»»»»»»»»»»~
~»»»»‹‹‹»»»»»»»»»»»»»‹‹‹»»»»»»»»»»»»»‹‹‹»»»»»»»‹‹‹»»»‹‹‹»»»»»»»‹‹‹»»»»»»»»»»»»~
~»»»»‹‹‹»»»»»»»»»»»»»‹‹‹‹‹‹‹‹‹‹‹‹‹»»»‹‹‹»»»»»»»‹‹‹»»»‹‹‹»»»»»»»‹‹‹»»»»»»»»»»»»~
~»»»»‹‹‹»»»»»»»»»»»»»‹‹‹‹‹‹‹‹‹‹‹‹‹»»»‹‹‹»»»»»»»‹‹‹»»»‹‹‹»»»»»»»‹‹‹»»»»»»»»»»»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
")

