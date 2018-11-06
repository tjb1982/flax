(defproject co.nclk/flax "2.3.0"
  :description "Clojure library: YAML as a Clojure dialect."
  :url "https://github.com/tjb1982/flax"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [stencil "0.5.0"]
                 [clj-ssh "0.5.11"]
                 ;;[circleci/clj-yaml "0.5.3"]
                 [co.nclk/clj-yaml "1.0.0"]
                 [cheshire "5.6.3"]
                 ]
  :aot :all
  :main co.nclk.flax.main
  )
