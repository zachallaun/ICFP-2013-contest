(ns com.hackerschool.icfp-contest-2013.core
  (:require [clj-http.client :as http]
            [clojure.pprint :refer [pprint]]
            [cheshire.core :as json]))

(def icfp-url
  "http://icfpc2013.cloudapp.net/")

(def api-key
  "0417IVU3bbugkcfbldUEEuBw4LfbMgFyHtF84Qyf")

(def auth-key
  (str api-key "vpsH1H"));;not sure why the contest organizers added this suffix.

(defn icfp-post
  "Make a POST request to the contest server."
  ([route]
     (icfp-post route {}))
  ([route data]
     (http/post (str icfp-url route)
                {:query-params {:auth auth-key}
                 :body (json/generate-string data)
                 :as :json})))

(defn icfp-get
  "Make a GET request to the contest server."
  [route]
  (http/get (str icfp-url route)
            {:query-params {:auth auth-key}
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

(defn my-problems-req
  []
  (icfp-post "myproblems"))

(def problems
  (map (fn [problem]
         (update-in problem [:operators] #(map symbol %)))
       (:body (my-problems-req))))

(defn eval-req
  "Make an eval request for program `id` given a sequence of inputs.
   (count inputs) <= 256"
  [id inputs]
  (:body (icfp-post {:id id :arguments (vec inputs)})))


(comment


  (training-problem)

  (take 2 (filter #(< (:size %) 4) problems))


  )
