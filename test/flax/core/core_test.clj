(ns flax.core.core-test
  (:require [clojure.test :refer :all]
            [clj-yaml.core :as yaml]
            [flax.core :as flax]
            [clojure.tools.logging :refer [*logger-factory*]]
            [clojure.tools.logging.impl :as logp]))

(defmacro do-while
  [test & [body]]
  `(loop [ex# ~body]
     (if-let [x# (~test ex#)]
       (recur x#)
       ex#)))


(def string-logger
  (reify logp/Logger
    (enabled? [logger level] true)
    (write! [logger level throwable msg]
      (println msg))))


(def string-logger-factory
  (reify logp/LoggerFactory
    (name [factory] "")
    (get-logger [factory logger-ns]
      string-logger)
    ))


(deftest test-pathwise
  (testing "Pathwise with resolved functions"
    (let [result (flax/evaluate
                   (yaml/parse-string "
                     (pathwise:
                     - »some«
                     - »true?«
                     - foo.bar.baz")
                   {:foo {:bar {:baz true :quux false}}})]
      (-> result true? is)))
  (testing "Pathwise with custom functions"
    (let [result (flax/evaluate
                   (yaml/parse-string "
                     (pathwise:
                     - (fn:
                       - [pred, things]
                       - (not:
                         - (apply:
                           - «pred
                           - [(nth: [~@things, 2]]
                     - »nil?«
                     - foo.bar.baz")
                   {:foo {:bar {:baz true :quux false}}})]
      (-> result true? is))))

(deftest test-literals
  (testing "String literals evaluate to strings unchanged"
    (let [string "foo bar baz"
          result
          (flax/evaluate
            (yaml/parse-string string))]
      (-> result string? is)
      (-> result (= string) is)))
  (testing "Keyword literals take the form \"«:foo\""
    (let [keyword* :foo
          result
          (flax/evaluate
            (yaml/parse-string "«:foo"))]
      (-> result keyword? is)
      (-> result (= keyword*) is)))
  (testing "Symbol literals take the form \"«'foo\""
    (let [symbol* 'foo
          result
          (flax/evaluate
            (yaml/parse-string "«'foo"))]
      (-> result symbol? is)
      (-> result (= symbol*) is)))
  (testing "Numbers without floating points become instances of java.lang.Integer"
    (let [long* 1
          result
          (flax/evaluate
            (yaml/parse-string "1"))]
      (-> result integer? is)
      (->> result (instance? java.lang.Integer) is)
      (-> result (= 1) is)))
  (testing "Numbers with floating points become instances of java.lang.Double"
    (let [long* 1.1
          result
          (flax/evaluate
            (yaml/parse-string "1.1"))]
      (-> result float? is)
      (->> result (instance? java.lang.Double) is)
      (-> result (= 1.1) is)))


  (testing "Functions"
    (let [fun (fn [x & more] x)
          result
          (flax/evaluate
            (yaml/parse-string "
              (fn: [[x], «x]"))]
      (-> (supers (class result)) (= (supers (class fun))) is)
      (-> (result 1) (= (fun 1)) is))
    (let [fun (fn [x & more] "x")
          result
          (flax/evaluate
            (yaml/parse-string "
              (fn:
              - [x]
              - x"))]
      (-> (result 1) (= (fun 1)) is)))
  (testing "Functions using `#`"
    (let [fun (fn [x & more] x)
          result
          (flax/evaluate
            (yaml/parse-string "
              (#:
              - «0"))]
      (-> (supers (class result)) (= (supers (class fun))) is)
      (-> (result 1) (= (fun 1)) is))
    (let [fun (fn [& x] "x")
          result
          (flax/evaluate
            (yaml/parse-string "
              (#:
              - x"))]
      (-> (result 1) (= (fun 1)) is)))


  (testing "«clj strings"
    (let [env {:foo {:bar "baz"}}
          result1
          (flax/evaluate
            (yaml/parse-string "
              foo.dog.chester.doodle: |
                  «clj (let [x «{pprint:foo}]
                         (-> x :bar))")
            env)
          result2
          (flax/evaluate (yaml/parse-string
                           "foo.dog.chester.doodle: «(identity «foo.bar»)")
                         env)
          result3
          (flax/evaluate (yaml/parse-string
                           "foo.dog: '«(:«{baz} «x»)'")
                         {:baz "quux" :x {:quux "lalala"}})]
      (-> result1 :foo :dog :chester :doodle (= "baz") is)
      (-> result2 :foo :dog :chester :doodle (= "baz") is)
      (-> result3 :foo :dog (= "lalala") is)))


  (testing "Environment"
    (let [env {:foo :bar}
          result
          (flax/evaluate
            (yaml/parse-string "«foo")
            env)]
      (-> result keyword? is)
      (-> result (= :bar) is))
    (let [env {:foo {:bar "baz"}}
          result
          (flax/evaluate
            (yaml/parse-string "«foo.bar")
            env)]
      (-> result string? is)
      (-> result (= "baz") is))
    (let [env {:foo [{:bar "baz"}]}
          result
          (flax/evaluate
            (yaml/parse-string "«foo.0.bar")
            env)]
      (-> result string? is)
      (-> result (= "baz") is))
    (let [env {:foo [{:bar "baz"}]}
          result
          (try
            (flax/evaluate
              (yaml/parse-string "«foo.lala")
              env)
            (catch RuntimeException re
              re))]
      (->> result (instance? RuntimeException) is)
      (->> result (do-while .getCause) (instance? NumberFormatException) is)
      )
    (let [env {:foo {:0 {:bar "baz"}}}
          result
          (flax/evaluate
            (yaml/parse-string "«foo.0.bar")
            env)]
      (-> result string? is)
      (-> result (= "baz") is))

    (binding [*logger-factory* string-logger-factory]
      (let [env {:foo {:bar "baz"}}
            result
            (flax/evaluate "«foo.quux" env)]
        (-> result nil? is)))
      
    ))
