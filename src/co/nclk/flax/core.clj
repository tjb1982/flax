(ns co.nclk.flax.core
  (:require [clj-yaml.core :as yaml]
            [clojure.tools.logging :refer [log]]
            [co.nclk.flax.data :as data]
            [cheshire.core :as json]
            [stencil.parser :refer [parse]]
            [stencil.core :refer [render]])
  ;; FIXME: does this belong here? Should there be a generic mechanism for 
  ;; importing other modules from various places? At the moment, I think it
  ;; sounds like a good idea to have this be a protocol with a single function
  ;; `resolve` and then let clients implement their own. Then we'd have to
  ;; provide some way of injecting it (rather than importing it here).
  (:import co.nclk.flax.data.FileDataConnector)
  (:gen-class))

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

(defn swap
  [s env]
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
    (let [s (subs s 2)
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


(defn evaluate
  [m & [config custom-eval regex-str]]
  (try
    (let [evaluate (or custom-eval evaluate)
          env (or (:env config) config)]
      (cond
        (string? m)
        (swap m env)

        (keyword? m)
        (keyword (swap (subs (str m) 1) env))

        (symbol? m)
        (symbol (swap (name m) env))

        (map? m)
        (cond

          ;; Functions and special forms
          (->> (keys m) (some #(-> % str (subs 1) (.startsWith "~("))))
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
            #_(let [sexp (conj
                         (->> (val fun-entry)
                              (map-indexed
                                #(let [item (evaluate %2 config)]
                                  (if (contains? #{'let 'fn 'binding 'for} fun)
                                    (if (= %1 0)
                                      (into [] item) item)
                                    (if (contains? #{'defn} fun)
                                      (if (= %1 1)
                                        (into [] item) item)
                                      item)))))
                         fun)]
              (eval sexp))
            (condp = fun
              ;; Special forms

              'clj
              (let [form (clojure.walk/postwalk
                           (fn [x]
                             (if (map? x)
                               (into {} x)
                               x))
                           (-> fun-entry val first (evaluate config) read-string))]
                (eval form))

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
              (let [parts (clojure.string/split (-> fun-entry val (nth 2) (evaluate config)) #"\.")
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
                                             (evaluate config)))
                                       %})]
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
                  yield))))

          ;; All other maps
          :else
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
                        (let [keyparts (clojure.string/split (name k) #"\.")
                              newkey (-> keyparts first keyword)]
                          (if (not-any? #(contains? % newkey) [nm env])
                            [k (evaluate v config)]
                            (let [oldval (or (get nm newkey) (get env newkey))
                                  selvec (->> keyparts
                                              (drop 1)
                                              (map #(try
                                                     (Integer/parseInt %)
                                                     (catch java.lang.NumberFormatException nfe
                                                       (keyword %))))
                                              (into []))]
                              (cond

                                (and (-> k name (.endsWith "."))
                                     (coll? v))
                                (let [ev (evaluate v (assoc config :env (merge env (get-in oldval selvec))))
                                      newval (reduce (fn [k]
                                                       (assoc-in oldval (conj selvec k)
                                                       "lalala")
                                                     oldval
                                                     (keys v)))]
                                  )
                                ;;;;

                                :else
                                ;; replace the base value with a new value with assoc-in
                                (let [newval (assoc-in oldval selvec (evaluate v config))]
                                  [newkey newval])))))

                        ;;(-> k name (.contains "."))
                        ;;(let [keyparts (clojure.string/split (name k) #"\.")
                        ;;      newkey (-> keyparts first keyword)
                        ;;      selvec (->> keyparts
                        ;;                  (drop 1)
                        ;;                  (map #(try
                        ;;                         (Integer/parseInt %)
                        ;;                         (catch java.lang.NumberFormatException nfe
                        ;;                           (keyword %))))
                        ;;                  (into []))
                        ;;      oldval (or (get nm newkey) (get env newkey))]
                        ;;  (if (nil? oldval)
                        ;;    [k v]
                        ;;    (cond
                        ;;      (and (-> k name (.endsWith "."))
                        ;;           (coll? v))
                        ;;      (let [ev (evaluate v (assoc config :env (merge env (get-in oldval selvec))))
                        ;;            newval (reduce (fn [k]
                        ;;                             (assoc-in oldval (conj selvec k)
                        ;;                             "lalala")
                        ;;                           oldval
                        ;;                           (keys v)))]
                        ;;        )
                        ;;      ;;;;

                        ;;      :else
                        ;;      ;; replace the base value with a new value with assoc-in
                        ;;      (let [newval (assoc-in oldval selvec (evaluate v config))]
                        ;;        [newkey newval]))))

                        :else [(evaluate k config) (evaluate v config)]))))
                {}
                m))))

        ;; Other collections
        (coll? m)
        (into (if (seq? m) [] (empty m))
              (doall (map (fn [item] (evaluate item config)) m)))

        :else m))
    (catch Exception e
      (when (or (map? m) (not (coll? m)))
        (println (type e))
        (log :error
          (with-out-str
            (clojure.pprint/pprint {:exception e
                                    :trace m})))
        (log :debug
          (with-out-str
            (println "\nEnvironment:")
            (clojure.pprint/pprint (:env config)))))
      (throw e))
    ))


(def ascii-art "
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»»»»*************»»»***»»»»»»»»»»»»»»»*********»»»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»*************»»»***»»»»»»»»»»»»»*************»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»***»»»»»»»»»»»»»***»»»»»»»***»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»*************»»»***»»»»»»»»»»»»»*************»»»»»***»»»***»»»»»»»»»»»»»»~
~»»»»*************»»»***»»»»»»»»»»»»»*************»»»»»»»*****»»»»»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»***»»»»»»»»»»»»»***»»»»»»»***»»»»»***»»»***»»»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»***»»»»»»»»»»»»»***»»»»»»»***»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»***»»»»»»»»»»»»»***»»»»»»»***»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»*************»»»***»»»»»»»***»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»***»»»»»»»»»»»»»*************»»»***»»»»»»»***»»»***»»»»»»»***»»»»»»»»»»»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»~
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
~»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»««»»~
")

