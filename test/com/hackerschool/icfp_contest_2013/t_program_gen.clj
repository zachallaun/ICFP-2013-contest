(ns com.hackerschool.icfp-contest-2013.t-program-gen
  (:require [midje.sweet :refer :all]
            [com.hackerschool.icfp-contest-2013.program-gen :refer :all]
            [clojure.core.logic :refer [run]]))

(def program-examples
  "Vector of known valid [program size opset] triples."
  '[[0 1 #{}]
    [x 1 #{}]
    [(if0 0 0 0) 4 #{if0}]
    [(if0 (if0 0 0 0) 0 0) 7 #{if0}]
    [(someop 0) 2 #{someop}]
    [(fold (op1 0) 1 (lambda (_ _) (op2 1 0))) 8 #{fold op1 op2}]])

(def sizeop
  (juxt size op))

(facts "Size and operator set of example programs are calculated correctly by `size` and `op`."
  (doseq [[p size opset] program-examples]
    (sizeop p) => [size opset]))

(facts "about core.logic runs"
  (fact
    (run 5 [q] (uniono [1 2 3] [3] q)) => '([1 2 3]))

  (fact
    (run 5 [q] (uniono [1 2 3] [4 5 6] q)) => '((4 5 6 1 2 3)))

  (fact
    (run 5 [q] (uniono [] [4 5 6] q)) => '([4 5 6] (4 5 6) (4 5 6) (4 5 6) (4 5 6)))

  (fact
    (run 10 [q] (uniono [4 5 6] q [1 2 3 4 5 6])) => '((1 2 3) (1 2 3 4) (1 2 3 5) (1 2 4 3) (1 2 3 4 4) (1 2 3 6) (1 4 2 3) (4 1 2 3) (1 2 3 4 5) (1 2 3 5 4)))
  
  (fact "1"
    (run 5 [q] (uniono [1 2 3] [3] q)) => '([1 2 3])))

(facts "about `operatorso`"
  (fact "the only programs w/o operators are 0 1 x y z"
    (let [programs (run 10 [q] (operatorso q ()))]
      (count programs) => 5
      (set programs) => #{0 1 'x 'y 'z})))

