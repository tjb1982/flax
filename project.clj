(defproject co.nclk/flax "4.0.0-SNAPSHOT"
  :description "Clojure library: YAML as a Clojure dialect."
  :url "https://github.com/tjb1982/flax"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [stencil "0.5.0"]
                 [clj-ssh "0.5.11"]
                 [co.nclk/clj-yaml "1.1.0-SNAPSHOT"]
                 ;;[cheshire "5.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 ]
  :aot :all
  ;:main co.nclk.flax.main
  )
