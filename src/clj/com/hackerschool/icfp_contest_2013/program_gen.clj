(ns com.hackerschool.icfp-contest-2013.program-gen
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.match :refer [match]]
            [clojure.set :refer [union]]))

;;                              Op 0 = {}
;;                              Op 1 = {}
;;                              Op x = {}
;;                 Op (if0 e0 e1 e2) = {"if0"}  U Op e0 U Op e1 U Op e2
;; Op (fold e0 e1 (lambda (x y) e2)) = {"fold"} U Op e0 U Op e1 U Op e2
;;                        Op (op1 e) = {op1}    U Op e0
;;                    Op (op2 e0 e1) = {op2}    U Op e0 U Op e1

(defn op [p]
  (match [p]
    [(:or 0 1)] #{}
    [(x :guard symbol?)] #{}
    [(['if0 e0 e1 e2] :seq)] (union #{:if0} (op e0) (op e1) (op e2))
    [(['fold e0 e1 (['lambda _ e2] :seq)] :seq)] (union #{:fold} (op e0) (op e1) (op e2))
    [([op1 e] :seq)] (union #{op1} (op e))
    [([op2 e0 e1] :seq)] (union #{op2} (op e0) (op e1))))


;; (defn with-operators
;;   [program ops]
;;   )



(comment

  (defn anyo [g]
    (conde
     [g]
     [(anyo g)]))

  (defn nevero []
    (anyo (== true false)))

  (run 1 [q]
    (nevero)
    fail)

  (run* [program]
    (operatorso program #{ops})
    (sizeo program 5)
    (runo program input out))

  )
