(defproject halleck "1.0.0-SNAPSHOT"
  :description "log4j appender"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [log4j "1.2.16"]]
  :dev-dependencies [[org.clojure/tools.logging "0.2.3"]]
  :aot #{halleck.core})
