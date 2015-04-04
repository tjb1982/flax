(ns co.nclk.flax.core
  (:require [clj-yaml.core :as yaml]
            [clojure.tools.logging :refer [log]]
            [co.nclk.flax.data :as data]
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

    ;; If `s` starts with ~@, then replace it with the data held by the
    ;; variable (i.e., not a string interpolation of it).
    ;; Supports '.' notation (e.g., foo.bar.0.baz etc.)
    (.startsWith s "~@")
    (let [target (-> s (subs 2))]
      (loop [target target env env]
        (cond
          (-> target (.contains "."))
          (let [parts (-> target (clojure.string/split #"\." 2))]
            ;; TODO support bracket sugar (e.g., foo[0].bar[baz])
            (recur (second parts)
                   (get-with-string-maybe-index (first parts) env)))
          :else
          (when-not (or (empty? env)
                        (clojure.string/blank? target))
            (let [data (get-with-string-maybe-index target env)]
              (if (string? data) (clojure.string/trim data) data))))))

    ;; If `s` starts with "~$", then replace it with the
    ;; stdout result of running the script locally.
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
    :else (render
            (parse s @parser-options)
            env)))


(defn evaluate
  [m & [env]]
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
            do-statements (fn [statements env]
                            (loop [statements statements last-ret nil]
                              (if (empty? statements)
                                last-ret
                                (let [ret (evaluate (first statements) env)]
                                  (recur (drop 1 statements) ret)))))]
        (condp = fun
          ;; Special forms

          'if
          (evaluate
            ((if (evaluate (-> fun-entry val first) env)
               second #(nth % 2 nil))
              (-> fun-entry val))
            env)

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
                                             env)
                                           (evaluate
                                             (second bindings)
                                             env)}))))]
                (do-statements statements env))))

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
                                              (keyword (first args)) env)
                                            (evaluate (first argv) env)})]
                            (recur (drop 1 args)
                                   (drop 1 argv)
                                   env))))]
              (do-statements statements env)))

          (symbol "#")
          (fn [& argv]
            (let [env (merge env
                             (into {}
                               (map-indexed
                                 (fn [idx item]
                                   [(keyword (str idx))
                                    (evaluate item env)])
                                 argv)))]
              (do-statements (-> fun-entry val) env)))

          'for
          (let [coll (-> fun-entry val first second (evaluate env))]
            (doall
              (map
                #(let [env (merge env
                                  {(keyword
                                     (-> fun-entry
                                         val
                                         ffirst
                                         (evaluate env)))
                                   %})]
                  (evaluate
                    (->> fun-entry val (drop 1)) env))
                coll)))

          'or
          (loop [args (-> fun-entry val)]
            (when-not (empty? args)
              (let [yield (evaluate (first args) env)]
                (if-not yield
                  (recur (drop 1 args))
                  yield))))

          'and
          (loop [args (-> fun-entry val)
                 last-yield nil]
            (if (empty? args)
              last-yield
              (let [yield (evaluate (first args) env)]
                (if-not yield
                  false
                  (recur (drop 1 args) yield)))))

          'parallel
          (upmap #(evaluate % env)
            (-> fun-entry val))

          ;; FIXME: rename this to something more indicative of what it does.
          'upmap
          (apply upmap (evaluate (-> m first val) env))

          'log
          (let [args (-> fun-entry val)]
            (log (evaluate (-> args first keyword) env)
                 (apply str
                   (map #(evaluate % env) (drop 1 args)))))


          ;; Functions
          (let [yield (apply (resolve fun)
                             (evaluate (-> m first val) env))]
            (if (coll? yield)
              ;; FIXME: Laziness disabled?
              (doall yield)
              yield))))

      ;; All other maps
      :else
      (into (empty m)
        (map (fn [[k v]]
               (if (= k :assert)
                 [k v]
                 [(evaluate k env) (evaluate v env)]))
             m)))

    ;; Other collections
    (coll? m)
    ((if (seq? m) reverse identity)
      (into (empty m) (doall (map (fn [item] (evaluate item env)) m))))

    :else m))


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

