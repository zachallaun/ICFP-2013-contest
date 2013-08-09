(ns com.hackerschool.icfp-contest-2013.t-program-gen
  (:require [midje.sweet :refer :all]
            [com.hackerschool.icfp-contest-2013.program-gen :refer :all]))

(def sizeop (juxt size op))

(def program-examples
  [[0  1 #{}]
   ['x 1 #{}]
   ['(if0 0 0 0) 4 #{'if0}]
   ['(if0 (if0 0 0 0) 0 0) 7 #{'if0}]
   ['(someop 0) 2 #{'someop}]
   ['(fold (op1 0) 1 (lambda (_ _) (op2 1 0))) 8 #{'fold 'op1 'op2}]])

(facts "about op"
  (doseq [[p size opset] program-examples]
    (sizeop p) => [size opset]))
