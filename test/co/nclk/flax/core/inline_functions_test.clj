(ns co.nclk.flax.core.inline-functions-test
  (:require [clojure.test :refer :all]
            [clj-yaml.core :as yaml]
            [co.nclk.flax.core :as flax]))

#_(deftest test-inline-functions
  (testing "Inline selectors behave like methods"
    (let [result (flax/evaluate
                   "~@foo.$str(world)"
                   {:foo "hello "})]
      (-> result (= "hello world") is))
    (let [env {:foo [1 2]}
          src "~@foo.#(->> % (map inc) (remove nil?))"
          expectation {:foo [2 3]}
          result (flax/evaluate src env)]
      (-> result (= expectation) is))
    ))

