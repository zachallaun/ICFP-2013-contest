(ns com.hackerschool.icfp-contest-2013.core
  (:require [clj-http.client :as client]
            [clojure.pprint :refer [pprint]]))

(def icfp-url
  "http://icfpc2013.cloudapp.net/")

(def api-key
  "0417IVU3bbugkcfbldUEEuBw4LfbMgFyHtF84Qyf")