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

(facts "about `uniono`"
  (run 5 [q] (uniono [1 2 3] [3] q)) => '([1 2 3])
  (run 5 [q] (uniono [1 2 3] [4 5 6] q)) => '((4 5 6 1 2 3))
  (run 5 [q] (uniono [] [4 5 6] q)) => '([4 5 6])
  (run 5 [q] (uniono [4 5 6] q [1 2 3 4 5 6])) => '((1 2 3) (1 2 3 4) (1 2 3 5) (1 2 4 3) (1 2 3 4 4))
  (run 5 [q] (uniono [1 2 3] [3] q)) => '([1 2 3])
  (run 1 [q] (uniono q [1] [])) => '())

(facts "about `operatorso`"
  (fact "the only programs w/o operators are 0 1 x y z"
    (let [programs (run 10 [q] (operatorso q ()))]
      (count programs) => 5
      (set programs) => #{0 1 'x 'y 'z}))

  #_(fact "programs with only one operator"
    (let [programs (run 10 [q] (operatorso q ['if0]))]
      (set programs) => #{0 1 'x 'y 'z}))

  )

