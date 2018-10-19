(ns co.nclk.flax.binding-test
  (:require [clojure.test :refer :all]
            [clj-yaml.core :as yaml]
            [co.nclk.flax.core :as flax]))


(defn yval
  [x & [env]]
  (-> x yaml/parse-string (flax/evaluate env)))


(deftest test-anonymous-functions


  (testing "Function in map"
    (let [src "
(map:
- (fn:
  - [idx]
  - «idx
- (range: [0,5]
"         result (yval src)]
      (-> result (= '(0 1 2 3 4)) is)))


  (testing "Function in map with environment variable as collection"
    (let [src "
(map:
- (fn:
  - [arg]
  - «arg
- «five-exes
"
          env {:five-exes ["x","x","x","x","x"]}
          result (yval src env)]
      (-> result (= '("x","x","x","x","x")) is)))


  (testing "Function in map with environment variable with inter-function scope"
    (let [src "
(map:
- (fn:
  - [arg]
  - «an-ex
- (range: [0,5]
"
          env {:an-ex "x"}
          result (yval src env)]
      (-> result (= '("x","x","x","x","x")) is)))


  (testing "Function in map with environment variable as collection"
    (let [src "
(map:
- (fn:
  - [arg]
  - «arg
- «five-exes
"
          env {:five-exes ["x","x","x","x","x"]}
          result (yval src env)]
      (-> result (= '("x","x","x","x","x")) is)))
  )
