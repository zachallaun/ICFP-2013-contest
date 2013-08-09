(ns com.hackerschool.icfp-contest-2013.t-program-gen
  (:require [midje.sweet :refer :all]
            [com.hackerschool.icfp-contest-2013.program-gen :refer :all]))

;;TODO: we should test op and size together
;; i.e.
;; (def sizeop (juxt size op))
;; (let [p '(if0 (if0 0 0 0) 0 0)]
;;   (sizeop p) => [2 #{:if0}])
(facts "about op"
  (op 0) => #{}
  (op 'x) => #{}
  (op '(if0 0 0 0)) => #{:if0}
  (op '(if0 (if0 0 0 0) 0 0)) => #{:if0})
