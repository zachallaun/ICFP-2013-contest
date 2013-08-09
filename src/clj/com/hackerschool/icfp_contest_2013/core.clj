(ns com.hackerschool.icfp-contest-2013.core
  (:require [clj-http.client :as http]
            [clojure.pprint :refer [pprint]]))

(def icfp-url
  "http://icfpc2013.cloudapp.net/")

(def api-key
  "0417IVU3bbugkcfbldUEEuBw4LfbMgFyHtF84Qyf")

(defn icfp-get
  "Make a GET request to the contest server."
  [route]
  (client/get (str icfp-url "/" route)
              {:query-params {;;not sure why the contest organizers added this suffix.
                              :auth (str api-key "vpsH1H")}
               :as :json}))

(defn training-problem
  "Get a training problem from the contest server; returns something like:
{:challenge '(lambda (x_23265) (plus (or (or (or (if0 (plus (xor (not x_23265) 0) 0) x_23265 x_23265) x_23265) x_23265) x_23265) x_23265))',
 :size 18,
 :operators ['if0' 'not' 'or' 'plus' 'xor'],
 :id 'h7oZTxtwiD00OapNDtCAstRG'}"
  []
  (-> (:body (icfp-get "train"))
      (select-keys [:id :size :operators :challenge])))



(comment

  (training-problem)
  
  )