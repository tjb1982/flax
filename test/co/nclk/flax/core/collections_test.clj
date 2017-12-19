(ns co.nclk.flax.core.collections-test
  (:require [clojure.test :refer :all]
            [clj-yaml.core :as yaml]
            [co.nclk.flax.core :as flax]))

(deftest test-literals
  (testing "Dictionary literals become clojure maps"
    (let [map* {:foo "bar" :baz 123 :quux {:lala "june"}}
          result
          (flax/evaluate
            (yaml/parse-string "
               foo: bar
               baz: 123
               quux:
                 lala: june"))]
      (-> result coll? is)
      (-> result associative? is)
      (-> result sequential? not is)
      (-> result map? is)
      (-> result (= map*) is)))
  (testing "Dict keys with escaped dots become keys with dots (and aren't expanded)"
    (let [map* {:foo {:bar.baz "quux"}
                :one.two.three {:four {:five 6} :seven {:eight 9}}}
          result
          (flax/evaluate
            (yaml/parse-string "
              foo.bar\\.baz: quux
              one\\.two\\.three:
                four.five: 6
                seven: {eight: 9}")
            {})]
      (-> result (= map*) is)))
    (let [map* {:foo {:bar.baz "hello world"}}
          result
          (flax/evaluate
            (yaml/parse-string "
              foo.bar\\.baz.: world")
            {:foo {:bar.baz "hello "}})]
      (-> result (= map*) is))
  (testing "End-dotted dictionary entries assoc to maps instead of replace"
    (let [map* {:foo {:bar {:baz "quux" :chester "dog" :june "dog"}}}
          result
          (flax/evaluate
            (yaml/parse-string "
              foo.bar.:
                chester: dog
                june: dog")
            {:foo {:bar {:baz "quux"}}})]
      (-> result (= map*) is)))
  (testing "End-dotted dictionary entries append to lists instead of replace"
    (let [map* {:foo {:bar ["baz", "quux"]}}
          result
          (flax/evaluate
            (yaml/parse-string "
              foo.bar.: quux")
            {:foo {:bar ["baz"]}})]
      (-> result (= map*) is)))
  (testing "End-dotted dictionary entries append to strings instead of replace"
    (let [map* {:foo {:bar "hello world"}}
          result
          (flax/evaluate
            (yaml/parse-string "
              foo.bar.: world")
            {:foo {:bar "hello "}})]
      (-> result (= map*) is)))
  (testing "End-dotted dictionary entries concat to lists instead of replace"
    (let [map* {:foo {:bar ["baz", "rachel", "tom"]}}
          result
          (flax/evaluate
            (yaml/parse-string "
              foo.bar.:
              - rachel
              - tom")
            {:foo {:bar ["baz"]}})]
      (-> result (= map*) is)))
  (testing "End-dotted dictionary entries can be nested"
    (let [map* {:foo {:bar ["baz", "rachel", "tom"]}}
          result
          (flax/evaluate
            (yaml/parse-string "
              foo.:
                bar.:
                - rachel
                - tom")
            {:foo {:bar ["baz"]}})]
      (-> result (= map*) is))
    (let [map* {:foo {:bar {:baz "quux" :rachel "tom"}}}
          result
          (flax/evaluate
            (yaml/parse-string "
              foo.:
                bar.:
                  rachel: tom")
            {:foo {:bar {:baz "quux"}}})]
      (-> result (= map*) is)))


  (testing "Dotted keys expand to nested maps"
    (let [map* {:foo {:bar {:baz "quux"}}}
          result
          (flax/evaluate
            (yaml/parse-string "foo.bar.baz: quux")
            {})]
      (-> result (= map*) is))
    (let [map* {:foo {:bar {:baz "quux"}}}
          result
          (flax/evaluate
            (yaml/parse-string
              "foo.bar:
                 baz: quux")
            {:env {:foo {:bar "baz"}}})]
      (-> result (= map*) is)))
  (testing "Dotted keys containing ints assoc to vector indices"
    (let [map* {:foo {:bar ["one" 2]}}
          result
          (flax/evaluate
            (yaml/parse-string
              "foo.bar.1: 2")
            {:env {:foo {:bar ["one" "two"]}}})]
      (-> result (= map*) is))
    (let [list** '(1 "two" three)
          result
          (flax/evaluate
            (yaml/parse-string "
              - 1
              - two
              - ~'three"))]
      (-> result coll? is)
      (-> result sequential? is)
      (-> result vector? is)
      (-> result count (= 3) is)
      (-> result (= list**) is))

    (let [map* {:foo {:bar ["one" 2]}}
          result
          (flax/evaluate
            (yaml/parse-string "
              ~(let:
              - - lala
                - foo.bar.1: three
                  foo.bar.1: four
              - ~@lala")
            {:env {:foo {:bar ["one" "two"]}}})]
      (-> result (= {:foo {:bar ["one" "four"]}}) is))))
