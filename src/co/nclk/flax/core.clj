(ns co.nclk.flax.core
  (:require [clj-yaml.core :as yaml]
            [clojure.tools.logging :refer [log]]
            [co.nclk.flax.data :as data]
            ;;[cheshire.core :as json]
            [clojure.data.json :as json]
            ;;[cheshire.generate :as cheshire]
            [stencil.parser :refer [parse]]
            [stencil.core :refer [render]])
  ;; FIXME: does this belong here? Should there be a generic mechanism for 
  ;; importing other modules from various places? At the moment, I think it
  ;; sounds like a good idea to have this be a protocol with a single function
  ;; `resolve` and then let clients implement their own. Then we'd have to
  ;; provide some way of injecting it (rather than importing it here).
  (:import co.nclk.flax.data.FileDataConnector)
  (:gen-class))

;;(cheshire/add-encoder clojure.lang.Var cheshire/encode-str)

(declare swap)
(declare evaluate)

(def parser-options (atom {:tag-open "«{" :tag-close "}"}))
(def ^:dynamic *pipeline* nil)

(defn config*
  [config & [newenv]]
  (let [config (if (contains? config :env)
                 config {:env config})]
    (if newenv
      (assoc config :env newenv)
      config)))

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


(defn pprint-handler
  [v]
  (-> v clojure.pprint/pprint
        with-out-str
        clojure.string/trim))

(defn eval-clj-string
  [s config]
  (let [form (clojure.walk/postwalk
               (fn [x]
                 (if (map? x)
                   (into {} x)
                   x))
               (-> s (swap config) read-string))]
    (eval form)))



(defn shell-interpolate
  [s]
  (let [[directive, cmd] (clojure.string/split s #"\s+" 2)
        file (-> directive (clojure.string/split #"\$") second)
        proc (-> (Runtime/getRuntime)
                 (.exec (into-array String
                          ["bash" "-c" (clojure.string/trim cmd)])))
        stream (clojure.java.io/reader
                 (condp = file
                   "out" (.getInputStream proc)
                   "stdout" (.getInputStream proc)
                   "err" (.getErrorStream proc)
                   "stderr" (.getErrorStream proc)
                   (.getInputStream proc)))
        ;; XXX: default 5 minutes. Maybe not so good.
        exited? (.waitFor proc 5 java.util.concurrent.TimeUnit/MINUTES)]
    (if (-> file (= "exit"))
      (if (true? exited?) (.exitValue proc) 124)
      (clojure.string/join "\n"
        (loop [lines []]
          (let [line (.readLine stream)]
            (if (nil? line)
              lines
              (recur (conj lines line)))))))))


(defn interpolable-value
  [label match env]
  (cond
    (= label :clj)
    (-> match (clojure.string/replace #"^«\{clj:|[\s+]?\}$" ""))

    (= label :sh)
    (-> match (clojure.string/replace #"^«\{sh:" "«\\$")
              (clojure.string/replace #"[\s+]?\}$" ""))

    (and (= label :pprint)
         (not (.startsWith match "«{")))
    (cond
      (and (-> match (.startsWith "«$"))
           (-> match (.endsWith "$»")))
      (shell-interpolate (-> match (clojure.string/replace #"\s*\$»$" "")))

      (and (.startsWith match "«(") (.endsWith match ")»"))
      (-> match (clojure.string/replace #"^«|»$" "") (eval-clj-string {:env env}))

      (and (.startsWith match "«¡") (.endsWith match "!»"))
      (-> match (clojure.string/replace #"^«¡\s*|\s*!»$" "") (dot-get env) yaml/generate-string)

      (and (.startsWith match "«¿") (.endsWith match "?»"))
      (-> match (clojure.string/replace #"^«¿\s*|\s*\?»$" "") (dot-get env) json/write-str)

      (and (.startsWith match "«")
           (.endsWith match "»"))
      (-> match (clojure.string/replace #"^«\s*|\s*»$" "") (dot-get env)))

    :else
    (-> match
        (subs (+ 3 (-> label name count))
              (- (count match) 1))
        (dot-get env))))


(defn interpolate
  [s label handler env]
  (let [regex (re-pattern
                (str "«\\{" (name label) ":.+?(?=\\})\\}"
                     (when (= label :pprint)
                       (str "|«¡.*?(?=!»)!»" ;; yaml string (escaped)
                            "|«¿.*?(?=\\?»)\\?»" ;; json string (escaped)
                            "|«\\(.+?(?=\\)»)\\)»" ;; clojure quote snippets
                            "|«\\$.+?(?=\\$»)\\$»" ;; shell snippets
                            "|«[^\\{].*?(?=»)»" ;; linen snippets
                            ))))]
    (loop [s s final ""]
      (if (clojure.string/blank? s)
        (str final s)
        (let [match (re-find regex s)]
          (if (nil? match)
            (recur nil (str final s))
            (let [v (interpolable-value label match env)
                  after-match-idx (+ (.indexOf s match) (.length match))]
              ;;(clojure.pprint/pprint ["TTT" match v])
              ;;(clojure.pprint/pprint (subs s 0 after-match-idx))
              (recur (subs s after-match-idx)
                     (str final (clojure.string/replace-first
                                  (subs s 0 after-match-idx)
                                  regex
                                  (clojure.string/re-quote-replacement
                                    (str (handler v)))))))))))))


(def ^:dynamic *env* nil)
(defn swap
  [s config]
  (let [env (or *env* (:env config))]
    (cond

      ;; Env dump.
      (= (clojure.string/trim s) "«*")
      env

      ;; Support for keyword literals.
      (.startsWith s "«:")
      (keyword (subs s 2))

      ;; Support for symbol literals.
      (.startsWith s "«'")
      (symbol (subs s 2))

      ;; Support for resolved symbol literals.
      (and (.startsWith s "»") (.endsWith s "«"))
      (resolve (symbol (subs s 1 (dec (count s)))))

      ;; If `s` starts with "~$", then replace it with the
      ;; stdout result of running the command locally.
      (and (.startsWith s "«$")
           (not (re-find #"\$»" (.trim s))))
      (shell-interpolate s)

      (.startsWith s "«clj")
      (eval-clj-string (subs s 4) config)

      ;; easier Clj interpolation.
      (let [s (clojure.string/trim s)]
        (and (.startsWith s "«(") (.endsWith s ")")))
      (eval-clj-string (subs s 1) config)

      ;; If `s` starts with «, then replace it with the data held by the
      ;; variable (i.e., not a string interpolation of it).
      ;; Supports '.' notation (e.g., foo.bar.0.baz etc.)
      (and (.startsWith s "«")
           (not (.startsWith s "«{"))
           (not (re-find #"»" (.trim s))))
      (let [target (-> s (subs 1))]
        ;; XXX: wtf; why (:env config) here where everyone else is using *env*?
        ;; I highly doubt that that was accidental
        (dot-get target env)) ;;(:env config)))

      ;; Interpolate.
      :else (-> s
              (interpolate :json json/write-str env)
              (interpolate :yaml yaml/generate-string env)
              (interpolate :sh shell-interpolate env)
              (interpolate :clj #(eval-clj-string % env) env)
              (interpolate :pprint pprint-handler env)
              (parse @parser-options)
              (render env)
              ))))

(defn evaluate-function-or-special-form
  [m config]
  (let [config (config* config)
        env (:env config)
        fun-entry (->> m
                       (filter
                         #(-> % key str (subs 1) (.startsWith "(")))
                       first)
        fun (-> fun-entry key str (subs 2) symbol)
        do-statements (fn [statements config pipeline]
                        (loop [statements statements last-ret nil]
                          (if (empty? statements)
                            last-ret
                            ;;(let [ret (binding [*env* (:env config)]
                            ;;            (pipeline (first statements) config))]
                            (let [ret (binding [*env* nil] (pipeline (first statements) config))]
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
      (eval-clj-string (-> fun-entry val first) config)

      'if
      (*pipeline*
        ((if (*pipeline* (-> fun-entry val first) config)
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
                                      (let [config (assoc config :env env)]
                                        {(*pipeline* (keyword (first bindings)) config)
                                         (*pipeline* (second bindings) config)})))))]
            (do-statements statements (assoc config :env env) *pipeline*))))

      'fn
      (let [pipeline *pipeline*]
        (fn [& argv]
          (let [args (-> fun-entry val first) ;; list of strings
                statements (->> fun-entry val (drop 1))
                env (loop [args args
                           argv argv
                           env env]
                      (if (empty? args)
                        env
                        (let [env (merge env
                                         {(pipeline
                                            (keyword (first args)) config)
                                          (pipeline (first argv) config)})]
                          (recur (drop 1 args)
                                 (drop 1 argv)
                                 env))))]
            ;; XXX no idea what this was supposed to accomplish, but it broke
            ;; almost everything.
            ;(binding [*env* env]
            (do-statements statements (assoc config :env env) pipeline))))

      (symbol "#")
      (let [pipeline *pipeline*]
        (fn [& argv]
          (let [env (merge env
                           (into {}
                             (map-indexed
                               (fn [idx item]
                                 [(keyword (str idx))
                                  (pipeline item config)])
                               argv)))]
            (do-statements (-> fun-entry val) (assoc config :env env) pipeline))))

      'pathwise
      (let [parts (clojure.string/split
                    (-> fun-entry val (nth 2) (*pipeline* config))
                    #"\.")
            cfun (-> fun-entry val first (*pipeline* config))
            pred (-> fun-entry val second (*pipeline* config))
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
      ;; FIXME: while this is fine for some things, any unreadable Java object will
      ;; throw an exception because "can't embed object in code"
      (let [args (-> fun-entry val)
            body (-> args last)
            bindings (-> args first)]
        (when-not (even? (count bindings))
          (throw (IllegalArgumentException.
                   (ex-info "`for` requires and even number of forms in its binding vector"
                            {:binding-vector bindings}))))
        (let [[newenv evaluated-bindings]
              (loop [env env result [] bindings bindings]
                (if (empty? bindings)
                  [env result]
                  (let [current-binding (take 2 bindings)
                        [cbkey cbsrc] current-binding
                        cbval (*pipeline* cbsrc env)
                        newenv (assoc env (keyword cbkey) cbval)]
                    (recur newenv (conj result [(symbol (str cbkey)) cbval])
                                  (drop 2 bindings)))))
              binding-keys (map first evaluated-bindings)
              binding-vector (vec (apply concat
                                         (map (fn [[f s]] [f `(quote ~s)])
                                              evaluated-bindings)))
              form `(for ~binding-vector
                     (let [local-env#
                           (loop [env# (quote ~env)
                                  bindings# (quote ~binding-keys)
                                  vals*# (list ~@binding-keys)]
                             (if (empty? bindings#)
                               env#
                               (let [key*# (keyword (first bindings#))
                                     val*# (first vals*#)]
                                 (recur (assoc env# key*# val*#)
                                        (drop 1 bindings#)
                                        (drop 1 vals*#)))))]
                     (evaluate (quote ~body) local-env#)))]
          ;;(clojure.pprint/pprint form)
          (eval form)))

      ;;'for
      ;;(let [coll (-> fun-entry val first second (*pipeline* config))]
      ;;  (doall
      ;;    (map
      ;;      #(let [env (merge env
      ;;                        {(keyword
      ;;                           (-> fun-entry
      ;;                               val
      ;;                               ffirst
      ;;                               (*pipeline* config))) %})]
      ;;        (*pipeline*
      ;;          (->> fun-entry val (drop 1)) (update-in config [:env] merge env)))
      ;;      coll)))

      'or
      (loop [args (-> fun-entry val)]
        (when-not (empty? args)
          (let [yield (*pipeline* (first args) config)]
            (if-not yield
              (recur (drop 1 args))
              yield))))

      'and
      (loop [args (-> fun-entry val)
             last-yield nil]
        (if (empty? args)
          last-yield
          (let [yield (*pipeline* (first args) config)]
            (if-not yield
              false
              (recur (drop 1 args) yield)))))

      'parallel
      (upmap #(*pipeline* % config)
        (-> fun-entry val))

      ;; FIXME: rename this to something more indicative of what it does.
      'upmap
      (apply upmap (*pipeline* (-> m first val) config))

      'log
      (let [args (-> fun-entry val)]
        (log (*pipeline* (-> args first keyword) config)
             (apply str
               (map #(with-out-str (clojure.pprint/pprint (*pipeline* % config)))
                    (drop 1 args)))))

      ;; Functions
      (let [funstr (str fun)
            delay-evaluation? (-> funstr (.startsWith "("))
            fun (if delay-evaluation?
                  (-> funstr (subs 1) symbol)
                  fun)
            args (-> m first val)
            evaluated-args (if delay-evaluation? args (*pipeline* args config))
            yield (try
                    (let [result (apply (resolve fun) evaluated-args)]
                      (if delay-evaluation?
                        (*pipeline* result config)
                        result))
                    (catch NullPointerException npe
                      (log :error (format (str "Possible missing function definition: "
                                               "Caught NullPointerException while calling "
                                               "function \"%s\" with arguments: %s")
                                          (name fun)
                                          (vec m)))
                        (throw npe)
                        ))]
        (if (coll? yield)
          ;; FIXME: Laziness disabled?
          (doall yield)
          yield)))))

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

(defn evaluate-end-dotted-entry
  [oldval selvec v]
  (let [oldleaf (get-in oldval selvec)
        newleaf
        (cond
          (map? oldleaf)
          (merge oldleaf v)

          (coll? oldleaf)
          (if (coll? v)
            (vec (concat oldleaf v))
            (conj oldleaf v))

          (string? oldleaf)
          (str oldleaf v)

          )]
    (if (empty? selvec)
      newleaf
      (assoc-in oldval selvec newleaf))))

(defn evaluate-dotted-existing-entry
  [m newkey selvec v oldval]
  (let [newval (if (-> selvec last nil?)
                 (evaluate-end-dotted-entry oldval (butlast selvec) v)
                 (assoc-in oldval selvec v))]
    [newkey newval]))

(defn evaluate-dotted-entry
  [m k v config]
  (let [keyparts (parse-keyparts k)
        newkey (-> keyparts first)
        env (:env (config* config))]
      (if (not-any? #(contains? % newkey) [m env])
        (assoc-in m keyparts (*pipeline* v config))
        (let [oldval (or (get m newkey) (get env newkey))]
          (evaluate-dotted-existing-entry m 
                                          newkey
                                          (drop 1 keyparts)
                                          (binding [*env* env]
                                            (*pipeline* v (config* config oldval)))
                                          oldval)))))

(defn evaluate-map-literal
  [m config]
  (if (record? m)
    m
    (into (empty m)
      (reduce
        (fn [nm [k v]]
          (let [k (*pipeline* k config)]
            (conj nm
              (cond
                ;; TODO: this doesn't belong here and isn't good.
                (= k :assert)
                [k v]

                (-> k name (.contains "."))
                (evaluate-dotted-entry nm k v config)

                ;; k is already evaluated above
                :else [k (*pipeline* v config)]))))
        {}
        m))))

(defn evaluate-map
  [m config]
  (cond
    ;; Functions and special forms
    (->> (keys m) (some #(-> % str (subs 1) (.startsWith "("))))
    (evaluate-function-or-special-form m config)

    ;; All other maps
    :else
    (evaluate-map-literal m config)))

(defn evaluate
  [m & [config pipeline]]
  (let [config (config* config)]
    (binding [*pipeline* (or pipeline *pipeline* evaluate)]
      (try
        (cond
          (string? m)
          (swap m config)

          (keyword? m)
          (keyword (swap (subs (str m) 1) config))

          (symbol? m)
          (symbol (swap (name m) config))

          (map? m)
          (evaluate-map m config)

          ;; Other collections
          (coll? m)
          (into (if (seq? m) [] (empty m))
                (doall (map (fn [item] (*pipeline* item config)) m)))

          :else m)
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
        (throw e)))

      ;;(catch Exception e
      ;;  (throw 
      ;;    (RuntimeException.
      ;;      #_(with-out-str
      ;;        (clojure.pprint/pprint {:config config}))
      ;;      e)))
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

