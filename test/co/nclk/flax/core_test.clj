(ns co.nclk.flax.core-test
  (:require [clojure.test :refer :all]
            [clj-yaml.core :as yaml]
            [co.nclk.flax.core :as flax]))

;;(deftest test-mutation-functionality
;;  (testing "Mutations via \".\""
;;    (-> false is)
;;    ))

(deftest test-pathwise
  (testing "Pathwise with resolved functions"
    (let [result (flax/evaluate
                   (yaml/parse-string "
~(pathwise:
- ~>'some
- ~>'true?
- foo.bar.baz
                     ")
                   {:foo {:bar {:baz true :quux false}}})]
      (-> result true? is)))
  (testing "Pathwise with custom functions"
    (let [result (flax/evaluate
                   (yaml/parse-string "
~(pathwise:
- ~(fn:
  - [pred, things]
  - ~(not:
    - ~(apply:
      - ~@pred
      - [~(nth: [~@things, 2]]
- ~>'nil?
- foo.bar.baz
                     ")
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
  (testing "Keyword literals take the form \"~:foo\""
    (let [keyword* :foo
          result
          (flax/evaluate
            (yaml/parse-string "~:foo"))]
      (-> result keyword? is)
      (-> result (= keyword*) is)))
  (testing "Symbol literals take the form \"~'foo\""
    (let [symbol* 'foo
          result
          (flax/evaluate
            (yaml/parse-string "~'foo"))]
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
  (testing "Dictionary literals become clojure maps"
    (let [map* {:foo "bar" :baz 123 :quux {:lala "june"}}
          result
          (flax/evaluate
            (yaml/parse-string
              "foo: bar\nbaz: 123\nquux:\n  lala: june"))]
      (-> result coll? is)
      (-> result associative? is)
      (-> result sequential? not is)
      (-> result map? is)
      (-> result (= map*) is)))
  (testing "Dict keys with escaped dots become keys with dots (and aren't expanded)"
    (let [map* {:foo {:bar.baz "quux"}}
          result
          (flax/evaluate
            (yaml/parse-string "foo.bar\\.baz: quux")
            {})]
      (-> result (= map*) is)))
  (testing "End-dotted dictionary entries assoc instead of replace"
    (let [map* {:foo {:bar {:baz "quux" :chester "dog" :june "dog"}}}
          result
          (flax/evaluate
            (yaml/parse-string "
foo.bar.:
  chester: dog
  june: dog
              ")
            {:foo {:bar {:baz "quux"}}})]
      (println result)
      (-> result (= map*) is)
      )
;;    (let [map* {:foo {:bar {:baz "quux"} :lalala "ksksks"}}
;;          result
;;          (flax/evaluate
;;            (yaml/parse-string "
;;foo.:
;;  lalala.: ksksks
;;  hahaha.:
;;    gfgfgf: 123
;;              ")
;;            {:foo {:bar {:baz "quux"}}})]
;;      (-> result (= map*) is))
    )



  (testing ""
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
              "foo.bar:\n  baz: quux")
            {:env {:foo {:bar "baz"}}})]
      (-> result (= map*) is))
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
            (yaml/parse-string
              "
- 1
- two
- ~'three
              "))]
      (-> result coll? is)
      (-> result sequential? is)
      (-> result vector? is)
      (-> result count (= 3) is)
      (-> result (= list**) is))

    (let [map* {:foo {:bar ["one" 2]}}
          result
          (flax/evaluate
            (yaml/parse-string
              "
~(let:
- - lala
  - foo.bar.1: three
    foo.bar.1: four
- ~@lala
              ")
            {:env {:foo {:bar ["one" "two"]}}})]
      (-> result (= {:foo {:bar ["one" "four"]}}) is))
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
      (-> (result 1) (= (fun 1)) is)))
  (testing "~clj strings"
    (let [env {:foo {:bar "baz"}}
          result
          (flax/evaluate
            (yaml/parse-string "
              foo.dog.chester.doodle: ~clj
                  (let [x ~{pprint:foo}]
                    (-> x :bar))")
            env)]
      (-> result :foo :dog :chester :doodle (= "baz") is)))
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
            (catch RuntimeException re
              (let [nfe (-> re .getCause)]
                nfe)))]
      (->> result (instance? NumberFormatException) is))
    (let [env {:foo {:0 {:bar "baz"}}}
          result
          (flax/evaluate
            (yaml/parse-string "~@foo.0.bar")
            env)]
      (-> result string? is)
      (-> result (= "baz") is))))
