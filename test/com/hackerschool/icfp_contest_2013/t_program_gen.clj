(ns com.hackerschool.icfp-contest-2013.t-program-gen
  (:require [midje.sweet :refer :all]
            [com.hackerschool.icfp-contest-2013.program-gen :refer :all]))

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
