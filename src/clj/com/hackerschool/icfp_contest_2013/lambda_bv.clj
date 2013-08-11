(ns com.hackerschool.icfp-contest-2013.lambda-bv
  (:refer-clojure :exclude [==])
  (:require [clojure.core.match :refer [match]]
            [clojure.core.logic :refer :all
             ]))

;; program    P ::= '(' 'lambda' '(' id ')' e ')'
;; expression e ::= '0' | '1' | id
;;               | '(' 'if0' e e e ')'
;;               | '(' 'fold' e e '(' 'lambda' '(' id id ')' e ')' ')'
;;               | '(' op1 e ')'
;;               | '(' op2 e e ')'
;;          op1 ::= 'not' | 'shl1' | 'shr1' | 'shr4' | 'shr16'
;;          op2 ::= 'and' | 'or' | 'xor' | 'plus'
;;          id  ::= [a-z]+

(defn- long? [n]
  (instance? java.lang.Long n))

(defn long->bytes
  "not really bytes, but byte-value longs"
  [l]
  (map (fn [b] (bit-and (long b) 0xFF))
       (-> (java.nio.ByteBuffer/allocate 8)
           (.putLong l)
           (.array))))

(defn fold
  "λBV fold operation"
  [n init f]
  (let [bytes (reverse (long->bytes n))]
    (reduce f init bytes)))

;; fixme: rename me.
(defn run-program
  "Runs a λBV program, given a long argument, and returns a long."
  [e arg]
  {:pre  [(long? arg)]
   :post [long?]}

  ;; Evaluate an expression.
  (letfn [(run-body [e env]
            (match [e]
              [(:or 0 1)] e

              ;;unary
              [(['not e1] :seq)]   (bit-not (run-body e1 env))
              [(['shl1 e1] :seq)]  (bit-shift-left (run-body e1 env) 1)
              [(['shr1 e1] :seq)]  (bit-shift-right (run-body e1 env) 1)
              [(['shr4 e1] :seq)]  (bit-shift-right (run-body e1 env) 4)
              [(['shr16 e1] :seq)] (bit-shift-right (run-body e1 env) 16)

              ;;binary
              [(['and e1 e2] :seq)] (bit-and (run-body e1 env) (run-body e2 env))
              [(['or e1 e2] :seq)]  (bit-or (run-body e1 env) (run-body e2 env))
              [(['xor e1 e2] :seq)] (bit-xor (run-body e1 env) (run-body e2 env))
              [(['plus e1 e2] :seq)] (+ (run-body e1 env) (run-body e2 env))

              ;;if0
              [(['if0 e0 e1 e2] :seq)]
              (if (= 0 (run-body e0 env))
                (run-body e1 env)
                (run-body e2 env))

              ;;fold
              [(['fold n init (['lambda ([x acc] :seq) body] :seq)] :seq)]
              (fold (run-body n env)
                    (run-body init env)
                    (fn [acc' x'] ;;switch arg order to be consistent with reduce
                      (run-body body (assoc env
                                       x x'
                                       acc acc'))))

              ;;env lookup
              [(id :guard symbol?)] (env id)))]
    (match [e]

      ;; Handle an entire program P
      [(['lambda ([id] :seq) body] :seq)]
      (run-body body {id arg}))))


;; Something like this?
(defn bit-noto [b out]
  (conde
   [(== b 1) (== 0 out)]
   [(== b 0) (== 1 out)]))

(comment
  (run 1 [q]
       (bit-noto 0 q))

  (run 2 [q]
       (bit-noto q 0))

  (run* [q]
    (fresh [u v]
      (bit-noto u v)
      (conso u v q)))
  )


(defn bitvector-noto [bv out]
  (conde
   [(emptyo bv) (== '() out)]
   [(fresh [fst rst]
      (conso fst rst bv)
      (fresh [flipb res]
        (bit-noto fst flipb)
        (conso flipb res out)
        (bitvector-noto rst res)))]))

;; todo: "shl1" | "shr1" | "shr4" | "shr16"
;; todo: "and" | "or" | "xor" | "plus"

;; assumes that input is a 64-element list
;; (defn bitvector-shl1o [bv out]
;;   (fresh [fstbit rstbits]
;;     (conso fstbit rstbits bv)
;;     (appendo rstbits 0 out)))


(defn run-bodyo [e env out]
  (conde
    [(== e 0) (== 0 out)]
    [(== e 1) (== 1 out)]

    ;; unary
    [(fresh [e1]
       (== (['not e1] :seq) e)
       (fresh [res]
         (run-bodyo e1 env res)
         (bitvector-noto res out)))]

    ;; TODO: etc.
    ))


         




(comment
  (lambda (x))

  (match ['(not x)]
    [(['not e] :seq)] :yep)

  (map inc
       )

(bit-or 0x0000000000000011
   (bit-or 0x0000000000000022
   (bit-or 0x0000000000000033
   (bit-or 0x0000000000000044
   (bit-or 0x0000000000000055
   (bit-or 0x0000000000000066
   (bit-or 0x0000000000000077
   (bit-or 0x0000000000000088
       0x0000000000000000))))))))
  )
