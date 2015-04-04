(ns co.nclk.flax.data
  (:require [clj-yaml.core :as yaml])
  (:gen-class))


(defprotocol PDataConnector
  (resolve [self m]))


(defn slurp-yaml
  [s]
  (let [s (if (.startsWith s (str "~" java.io.File/separator))
            (clojure.string/replace
              s #"~" (str (System/getProperty "user.home")))
            s)]
    (-> s slurp yaml/parse-string)))


(defrecord FileDataConnector []
  PDataConnector
  (resolve [self s]
    (slurp-yaml s)))
 

(defn connector [] (FileDataConnector.))

