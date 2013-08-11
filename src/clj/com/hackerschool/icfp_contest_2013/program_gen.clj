(ns com.hackerschool.icfp-contest-2013.program-gen
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.match :refer [match]]
            [clojure.set :refer [union]]))

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
      (conso a d ls) ;; unify (a d) with ls  
      (!= a elem)
      (not-membero elem d))]))

(defn uniono [x y out]
  (conde
    ;; if y is an empty set, then x is the union of x and y.
    [(emptyo y) (== x out)]
    [(emptyo x) (== y out)]
    [(emptyo out) (emptyo x) (emptyo y)]

    ;; otherwise, y is a pair of a and d.
    [(!= x []) (!= y []) (!= out [])
     (fresh [a d]
       (conso a d y) ;a=3, d=()
       (conde
         ;; if a is a member of x, then we can "peel off" a,
         ;; and take the union of x and d.
         [(membero a x) (uniono x d out)]

         ;; otherwise, we need to be sure to include a in the output.
         [(not-membero a x)
          (fresh [res]
            (conso a res out)
            (uniono x d res))]))]))

(comment
  ;; diverges, apparently :(
  (run 3 [q]
    (uniono q [1] [1 2 3]))

  )

(defn operatorso [p ops]
  (conde
    [(== p 0) (== ops ())]
    [(== p 1) (== ops ())]
    [(membero p identifiers) (== ops ())]

    ;; if0
    [(membero 'if0 ops)
     (fresh [e0 e1 e2]
       (== p ['if0 e0 e1 e2])
       (fresh [res0 res1 res2 tmp1 tmp2]
         (uniono res0 res1 tmp1)
         (uniono tmp1 res2 tmp2)
         (uniono tmp2 ['if0] ops)
         (operatorso e0 res0)
         (operatorso e1 res1)
         (operatorso e2 res2)))]

    ;; fold
    ;; TODO: tfold (top-level fold)
    [(membero 'fold ops)
     (fresh [e0 e1 e2 id]
       (== p ['fold e0 e1 ['lambda [id] e2]])
       (fresh [res0 res1 res2 tmp1 tmp2]
         (uniono res0 res1 tmp1)
         (uniono tmp1 res2 tmp2)
         (uniono tmp2 ['fold] ops)
         (operatorso e0 res0)
         (operatorso e1 res1)
         (operatorso e2 res2)))]

    ;; unary ops
    [(fresh [op e0 res]
       (== p [op e0])
       (conde
         [(== op 'not)
          (membero 'not ops)]
         [(== op 'shl1)
          (membero 'shl1 ops)]
         [(== op 'shr1)
          (membero 'shr1 ops)]
         [(== op 'shr4)
          (membero 'shr4 ops)]
         [(== op 'shr16)
          (membero 'shr16 ops)])
       (uniono res [op] ops)
       (operatorso e0 res))]

    ;; binary ops
    [(fresh [op e0 e1]
       (== p [op e0 e1])
       (conde
         [(== op 'and)
          (membero 'and ops)]
         [(== op 'or)
          (membero 'or ops)]
         [(== op 'xor)
          (membero 'xor ops)]
         [(== op 'plus)
          (membero 'plus ops)])
       (fresh [res0 res1 tmp]
         ;; make sure that op is part of ops
         (uniono res0 res1 tmp)
         (uniono tmp [op] ops)
         (operatorso e0 res0)
         (operatorso e1 res1)))]))

(comment
  ;; example: generate 10 expressions containing plus and xor
  (run 10 [q]
    (operatorso q ['plus 'xor]))

  (run 10 [q]
    (operatorso q ['if0]))

  (run 10 [q]
    (operatorso q ['fold]))

  )



(defn op
  "A set of the operators in the provided list expression, via the contest rules:

                             Op 0 = {}
                             Op 1 = {}
                             Op x = {}
                Op (if0 e0 e1 e2) = {'if0'}  U Op e0 U Op e1 U Op e2
Op (fold e0 e1 (lambda (x y) e2)) = {'fold'} U Op e0 U Op e1 U Op e2
                       Op (op1 e) = {op1}    U Op e0
                   Op (op2 e0 e1) = {op2}    U Op e0 U Op e1
"
  [p]
  (match [p]
    [(:or 0 1)] #{}
    [(_ :guard symbol?)] #{}
    [(['if0 e0 e1 e2] :seq)] (union #{'if0} (op e0) (op e1) (op e2))
    [(['fold e0 e1 (['lambda _ e2] :seq)] :seq)] (union #{'fold} (op e0) (op e1) (op e2))
    [([op1 e] :seq)] (union #{op1} (op e))
    [([op2 e0 e1] :seq)] (union #{op2} (op e0) (op e1))))


(defn size
  "The size of the provided list expression, via the contest rules:

                              |0| = 1
                              |1| = 1
                              |x| = 1
                 |(if0 e0 e1 e2)| = 1 + |e0| + |e1| + |e2|
 |(fold e0 e1 (lambda (x y) e2))| = 2 + |e0| + |e1| + |e2|
                       |(op1 e0)| = 1 + |e0|
                    |(op2 e0 e1)| = 1 + |e0| + |e1|
                 |(lambda (x) e)| = 1 + |e|
"
  [p]
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


(defn sizeo
  "Unifies `out` with a vector of 'a's that's the length of the size
  of the program `p`."
  [p out]
  (conde
    [(== p 0) (== out ['a])]
    [(== p 1) (== out ['a])]
    [(membero p identifiers) (== out ['a])]

    ;; if0
    [(fresh [e0 e1 e2]
       (== p ['if0 e0 e1 e2])
       (fresh [res0 res1 res2 tmp1 tmp2]
         (appendo res0 res1 tmp1)
         (appendo tmp1 res2 tmp2)
         (appendo ['a] tmp2 out)
         (sizeo e0 res0)
         (sizeo e1 res1)
         (sizeo e2 res2)))]

    ;; fold
    [(fresh [e0 e1 e2 id]
       (== p ['fold e0 e1 ['lambda [id] e2]])
       (fresh [res0 res1 res2 tmp1 tmp2 tmp3]
         (appendo res0 res1 tmp1)
         (appendo tmp1 res2 tmp2)
         (appendo ['a] tmp2 tmp3)
         (appendo ['a] tmp3 out)
         (sizeo e0 res0)
         (sizeo e1 res1)
         (sizeo e2 res2)))]

    ;; unary ops
    [(fresh [op e0 res]
       (== p [op e0])
       (conde
        [(== op 'not)]
        [(== op 'shl1)]
        [(== op 'shr1)]
        [(== op 'shr4)]
        [(== op 'shr16)])
       (appendo ['a] res out)
       (sizeo e0 res))]

    ;; binary ops
    [(fresh [op e0 e1]
       (== p [op e0 e1])
       (conde
        [(== op 'and)]
        [(== op 'or)]
        [(== op 'xor)]
        [(== op 'plus)])
       (fresh [res0 res1 tmp]
         ;; make sure that op is part of ops
         (appendo res0 res1 tmp)
         (appendo ['a] tmp out)
         (sizeo e0 res0)
         (sizeo e1 res1)))]))

(defn make-size [n]
  (cond
   (= n 0) []
   :else (vec (cons 'a (make-size (dec n))))))

(comment

  (make-size 4)

  (run 1 [q] (sizeo 0 q))

  (run 3 [q] (sizeo '[not 1] q))

  (run 1 [q] (sizeo '[and 0 1] q))

  (run 1 [q] (sizeo '[and [and 0 1] 0] q))

  (run 1 [q] (sizeo '[if0 0 0 0] q))

  (run 4 [q] (sizeo q '[a a a a]))

  (run 4 [q] (sizeo q (make-size 4)))

  (run 5 [q] (sizeo q (make-size 5)) (operatorso q ['and]))

  (run* [q] (sizeo q (make-size 2)))

  (run* [q] (sizeo q (make-size 3)))

  (run 1 [program]
    (sizeo program (make-size 6))
    (operatorso program '[not]))

  ;; Sigh.  This one happens to work, but it diverges arbitrarily
  ;; depending on the operators we pick.
  (run 1 [program]
    (sizeo program (make-size 6))

    ;; An attempt to make sure the order of operatorso's ops result
    ;; doesn't matter.  Don't know if it really helps.
    (fresh [ops]
      (operatorso program ops)
      (uniono ops '[if0 and] '[if0 and])))

  ;; An example of what we wish we could do...
  (run* [program]
    (operatorso program [if0 shl1])
    (sizeo program 5)
    (runo program input out))

  )
