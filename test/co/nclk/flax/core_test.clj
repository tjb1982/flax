(ns co.nclk.flax.core-test
  (:require [clojure.test :refer :all]
            [clj-yaml.core :as yaml]
            [co.nclk.flax.node :refer [node-manager]]
            [co.nclk.flax.core :as flax]))


(deftest test-basic-functionality
  (testing "~(log:"
    (let [result (flax/evaluate
                   (yaml/parse-string
                     "~(log:
                      - ~:info
                      - foo bar baz"), {})]
      (-> result nil? is)))
  (testing "Literals"
    (let [string "foo bar baz"
          result
          (flax/evaluate
            (yaml/parse-string string))]
      (-> result string? is)
      (-> result (= string) is))
    (let [keyword* :foo
          result
          (flax/evaluate
            (yaml/parse-string "~:foo"))]
      (-> result keyword? is)
      (-> result (= keyword*) is))
    (let [symbol* 'foo
          result
          (flax/evaluate
            (yaml/parse-string "~'foo"))]
      (-> result symbol? is)
      (-> result (= symbol*) is))
    (let [long* 1
          result
          (flax/evaluate
            (yaml/parse-string "1"))]
      (-> result integer? is)
      (->> result (instance? java.lang.Integer) is)
      (-> result (= 1) is))
    (let [map* {:foo "bar" :baz 123}
          result
          (flax/evaluate
            (yaml/parse-string
              "foo: bar\nbaz: 123"))]
      (-> result coll? is)
      (-> result associative? is)
      (-> result sequential? not is)
      (-> result map? is)
      (-> result (= map*) is))
    (let [list** '(1 "two" three)
          result
          (flax/evaluate
            (yaml/parse-string
              "
- 1
- two
- ~'three
              "))]
      (-> result coll? is)
      (-> result sequential? is)
      (-> result list? is)
      (-> result count (= 3) is)
      (-> result (= list**) is))
    )

  (testing "Functions"
    (let [fun (fn [x & more] x)
          result
          (flax/evaluate
            (yaml/parse-string
              "
~(fn:
- [x]
- ~@x
              "))]
      (-> (supers (class result)) (= (supers (class fun))) is)
      (-> (result 1) (= (fun 1)) is))
    (let [fun (fn [x & more] "x")
          result
          (flax/evaluate
            (yaml/parse-string
              "
~(fn:
- [x]
- x
              "))]
      (-> (result 1) (= (fun 1)) is))
    )
  (testing "Environment"
    (let [env {:foo :bar}
          result
          (flax/evaluate
            (yaml/parse-string "~@foo")
            env)]
      (-> result keyword? is)
      (-> result (= :bar) is))
    (let [env {:foo {:bar "baz"}}
          result
          (flax/evaluate
            (yaml/parse-string "~@foo.bar")
            env)]
      (-> result string? is)
      (-> result (= "baz") is))
    (let [env {:foo [{:bar "baz"}]}
          result
          (flax/evaluate
            (yaml/parse-string "~@foo.0.bar")
            env)]
      (-> result string? is)
      (-> result (= "baz") is))
    (let [env {:foo [{:bar "baz"}]}
          result
          (try
            (flax/evaluate
              (yaml/parse-string "~@foo.lala")
              env)
            (catch NumberFormatException nfe
              nfe))]
      (->> result (instance? NumberFormatException) is))
    (let [env {:foo {:0 {:bar "baz"}}}
          result
          (flax/evaluate
            (yaml/parse-string "~@foo.0.bar")
            env)]
      (-> result string? is)
      (-> result (= "baz") is))
    )
  )
