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

;; we only ever need 3 identifiers maximum in a lambda-bv program
(def identifiers ['x 'y 'z])

(defn -union [x y]
  (cond
    (empty? y) x
    (some #{(first y)} x) (-union x (rest y))
    :else (-union (cons (first y) x) (rest y))))

(defn removedo [x ls out]
  )

;; for comparison's sake
(defn not-member [elem ls]
  (cond
   (empty? ls) 't
   (= (first ls) elem) 'f
   :else (not-member elem (rest ls))))

;; a working relational not-membero.  succeeds if elem isn't a member of ls
(defn not-membero [elem ls]
  (conde
   [(emptyo ls)] ;; just succeed if ls is empty
   [(fresh [a d]
      (conso a d ls)
      (!= a elem)
      (not-membero elem d))]))

(comment
  (run 1 [q]
       (not-membero 1 [2 3]))

  (run 1 [q]
       (not-membero 'a ['b 'c]))

  (run 1 [q]
       (not-membero 'a []))

  (run 1 [q]
       (not-membero 'a ['a 'b 'a 'c]))

  (run 1 [q]
       (not-membero 'a ['a 'b 'c])))


(defn uniono [x y out]
  (conde
   ;; if y is an empty set, then x is the union of x and y.
    [(emptyo y) (== x out)]

    ;; otherwise, y is a pair of a and d.
    [(fresh [a d]
       (conso a d y) ;a=3, d=()
       (conde
         ;; if a is a member of x, then we can "peel off" a,
         ;; and take the union of x and d.
         [(membero a x) (uniono x d out)]

         ;; otherwise, we need to be sure to include a in the output.
         [(not-membero a x)
          (fresh [res]
            (conso a res out)
            (uniono x d res))]
         ))]))
(comment
  (run 5 [q]
       (uniono [1 2 3] [3] q))

  (run 5 [q]
       (uniono [1 2 3] [4 5 6] q))

  (run 5 [q]
       (uniono [] [4 5 6] q))

  (run 5 [q]
       (uniono [4 5 6] q [1 2 3 4 5 6]))

  ;; oh shit, it gives me a freaking OOM error!
  (run 5 [q]
       (uniono q [1] []))

  ;; oh shit, so does this!
  (run 5 [q]
       (uniono q [1] [1 2 3]))

  ((3 1 2 3) [1 2 3]))



(defn operatorso [p ops]
  (conde
    [(== p 0) (== ops ())]
    [(== p 1) (== ops ())]
    [(membero identifiers p) (== ops ())]

    ;; if
    [(fresh [e0 e1 e2]
       (== p ['if0 e0 e1 e2])
       (fresh [res0 res1 res2]
         (operatorso e0 res0)
         (operatorso e1 res1)
         (operatorso e2 res2)
         (fresh [tmp]
           (uniono res0 res1 tmp)
           (uniono res2 tmp ops))))]

    ;; fold
    [(fresh [e0 e1 e2 id]
       (== p ['fold e0 e1 (['lambda (id) e2] :seq)] :seq)
       (fresh [res0 res1 res2]
         (operatorso e0 res0)
         (operatorso e1 res1)
         (operatorso e2 res2)
         (fresh [tmp]
           (uniono res0 res1 tmp)
           (uniono res2 tmp ops))))]

    ;; unary ops
    [(fresh [op e0]
       (== p [op e0])
       (conde
        [(== op 'not)]
        [(== op 'shl1)]
        [(== op 'shr1)]
        [(== op 'shr4)]
        [(== op 'shr16)])
       (operatorso e0 ops))]

    ;; binary ops
    [(fresh [op e0 e1]
       (== p [op e0 e1])
       (conde
        [(== op 'and)]
        [(== op 'or)]
        [(== op 'xor)]
        [(== op 'plus)])
       (fresh [res0 res1]
         (operatorso e0 res0)
         (operatorso e1 res1)
         (uniono res0 res1 ops)))]))

(comment
  ;; Trying to generate some expressions that have no operators
  (run 3 [q]
       (operatorso q ()))

  (run 1 [q]
       (fresh [tmp]
              (appendo [1 2 3] [4 5 6] tmp)
              (appendo tmp [7 8 9] q))))
;; ((1 2 3 4 5 6 7 8 9))


(defn op [p]
  (match [p]
    [(:or 0 1)] #{}
    [(_ :guard symbol?)] #{}
    [(['if0 e0 e1 e2] :seq)] (union #{'if0} (op e0) (op e1) (op e2))
    [(['fold e0 e1 (['lambda _ e2] :seq)] :seq)] (union #{'fold} (op e0) (op e1) (op e2))
    [([op1 e] :seq)] (union #{op1} (op e))
    [([op2 e0 e1] :seq)] (union #{op2} (op e0) (op e1))))

;;                              |0| = 1
;;                              |1| = 1
;;                              |x| = 1
;;                 |(if0 e0 e1 e2)| = 1 + |e0| + |e1| + |e2|
;; |(fold e0 e1 (lambda (x y) e2))| = 2 + |e0| + |e1| + |e2|
;;                       |(op1 e0)| = 1 + |e0|
;;                    |(op2 e0 e1)| = 1 + |e0| + |e1|
;;                 |(lambda (x) e)| = 1 + |e|

(defn size [p]
  (match [p]
    [(:or 0 1)] 1
    [(_ :guard symbol?)] 1
    [(['if0 e0 e1 e2] :seq)] (+ 1 (size e0) (size e1) (size e2))
    [(['fold e0 e1 (['lambda _ e2] :seq)] :seq)] (+ 2 (size e0) (size e1) (size e2))
    [([op1 e] :seq)] (+ 1 (size e))
    [([op2 e0 e1] :seq)] (+ 1 (size e0) (size e1))
    [(['lambda _ e] :seq)] (+ 1 (size e))))

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
    (operatorso program '('if0 'shl1))
    (sizeo program 5)
    (runo program input out))

  )
