(defproject com.hackerschool/icfp-contest-2013 "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.7.6"]
                 [cheshire "5.1.2"]
                 [org.clojure/core.match "0.2.0-rc5"]]

  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[midje "1.5.1"]]}}

  :min-lein-version "2.0.0"

  :source-paths ["src/clj"])
