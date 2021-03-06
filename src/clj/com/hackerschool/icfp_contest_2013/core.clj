(ns com.hackerschool.icfp-contest-2013.core
  (:require [clj-http.client :as http]
            [clojure.pprint :refer [pprint]]
            [cheshire.core :as json]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [com.hackerschool.icfp-contest-2013.generate-bv :refer [rand-longs find-solution generate-programs]]
            [clojure.java.shell :refer [sh]]
            [clojure.set :as set]))

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
     (try
       (http/post (str icfp-url route)
                  {:query-params {:auth auth-key}
                   :body (json/generate-string data)
                   :as :json})
       (catch Exception e
         (case (:status (:object (.data e)))
           (412 410) (println "  GONE")
           429 (do (Thread/sleep 1000)
                   (icfp-post route data))
           (println "Don't know what happened"))))))

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

(defn myproblems-req
  []
  (icfp-post "myproblems"))

(defn eval-req
  "Make an eval request for program `id` given a sequence of inputs.
   (count inputs) <= 256"
  [id inputs]
  (:body (icfp-post "eval" {:id id :arguments (vec inputs)})))

(defn guess-req
  "Make a guess for program `id`"
  [id program]
  (:body (icfp-post "guess" {:id id :program program})))

(defn status-req
  "Request status"
  []
  (:body (icfp-post "status")))

;; (defn unchecked-long
;;   "Casts numbers that might overflow (e.g. BigInt) to long"
;;   [bi]
;;   (if (= (class bi) clojure.lang.BigInt)
;;     (.longValue (.bipart bi))
;;     (long bi)))

(defn hexstr
  "given a number, returns a hex string of the form '0x0000000000000000'"
  [n]
  (let [unpadded-str (Long/toHexString n)
        cnt (count unpadded-str)]
    (str "0x"
         (apply str (repeat (- 16 cnt) "0"))
         (str/upper-case unpadded-str))))

(defn program->str
  "'[lambda [x] [plus x 1]] => \"(lambda (x) (plus x 1)\""
  [program]
  (-> (str program)
      (str/replace "[" "(")
      (str/replace "]" ")")))

(defn icfp-oracle
  [id]
  (fn [& args]
    (match (vec args)
      [:examples] (let [ins (concat [0 -1] (take 254 (rand-longs)))
                        resp (eval-req id (map hexstr ins))]
                    (when (= (resp :status) "ok")
                      (zipmap ins (map (comp unchecked-long read-string)
                                       (resp :outputs)))))
      [:submit attempt] (let [source (program->str attempt)
                              resp (guess-req id source)]
                          (match [resp]
                            [{:status "win"}] [:win]
                            [{:status "error" :message msg}] [:error msg]
                            [{:status "mismatch" :values vals}]
                            (do
                              (println "  WRONG: " attempt)
                              (let [[in out] (map (comp unchecked-long read-string) vals)]
                                (println "  DISTINGUISHING INPUT: " in " => " out)
                                [:mismatch in out])))))))

;; (match [result]
;;   [[:win]] attempt
;;   [[:mismatch in out]] (recur {in out} (rest culled))
;;   [[:error msg]] (do (println (str "ERROR: " msg))
;;                      (recur (oracle :examples) culled)))

(defn do-problem
  "Given a problem from the icfp site, generate all possible solutions
   and try to find a solution using the icfp-oracle. Does all the things."
  [{:keys [id operators size] :as problem}]
  (println (str "WORKING ON: " problem))
  (let [solution (find-solution (generate-programs size operators)
                                (icfp-oracle id))]
    (println (str "  RESULT: " solution))
    solution))

(defn solvable?
  [problem]
  (not (contains? problem :solved)))

(defn size-n-problems
  [problems n]
  (filter #(= (:size %) n) problems))

(defn win! []
  (sh "say" "-v" "Good News" "win"))

(defn fail! []
  (sh "say" "-v" "Bad News" "fail"))

(defn do-problems [problems]
  (doseq [problem problems]
    (if (do-problem problem)
      (win!)
      (fail!))))

(comment

  (try
    (http/get "http://google.com/foobar")
    (catch Exception e
      (:status (:object (.data e)))))

  (def problems
    (->> (:body (myproblems-req))
         (map (fn [problem]
                (update-in problem [:operators] #(map symbol %))))
         #_(filter (fn [{:keys [operators]}]
                   (not (let [in-ops (set operators)]
                          (or (in-ops 'fold)
                              (in-ops 'tfold)))))) ;; we don't handle fold/tfold yet
         (filter solvable?)))

  (def only-unary-operators-problems
    (filter
     (fn [p]
       (set/subset? (set (:operators p)) (set '[not shl1 shr1 shr4 shr16])))
     problems))

  (defn n-operator-problems
    [n]
    (filter (fn [p] (= (count (:operators p)) n)) problems))

  (def two-operator-problems (n-operator-problems 2))

  (def single-unary-operator-problems
    (filter (fn [p]
              (== (count (:operators p)) 1))
            problems))

  (def size-3-problems
    (filter #(= (:size %) 3) problems))
  (count size-3-problems)



  (doseq [problem (size-n-problems 6)]
    (if-let [res (do-problem problem)]
      (do (println (str "RESULT: " res))
          (win!))
      (fail!)))

  (take 2 )

  ({:id "6QaBeiw9UA5f0Zkgc5fZQNOt", :size 3, :operators (shr1)} {:id "6RX515PBdDpEkRPGTjTTg6dU", :size 3, :operators (shr16)})



  (sh "open spotify:track:0k1xMUwn9sb7bZiqdT9ygx")

  )
