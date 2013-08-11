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
              [(['plus e1 e2] :seq)] (unchecked-add ^long (run-body e1 env) ^long (run-body e2 env))

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

;; ===============================================

;; Bitwise operations.

(defn bito
  "Relational predicate that succeeds if b is a bit."
  [b]
  (conde
   [(== b 1)]
   [(== b 0)]))

(defn bit-noto [b out]
  (conde
   [(== b 1) (== 0 out)]
   [(== b 0) (== 1 out)]))

(defn bit-ando
  "Relational predicate that succeeds if b1 and b2 are both 1."
  [b1 b2]
  (fresh []
    (== b1 1)
    (== b2 1)))

(defn bit-oro
  "Relational predicate that succeeds if at least one of b1 and b2 is 1."
  [b1 b2]
  (conde
   [(== b1 1)]
   [(== b2 1)]))

(defn bit-xoro
  "Relational predicate that succeeds if exactly one of b1 and b2 is 1."
  [b1 b2]
  (conde
   [(== b1 1) (== b2 0)]
   [(== b2 1) (== b1 0)]))

;; Bitvector operations.

;; These operations all assume that their input is a bitvector (vector
;; of {0, 1}) and that the result is a bitvector of the same length.

(defn bitvector-noto [bv out]
  (conde
   [(emptyo bv) (== '() out)]
   [(fresh [fst rst]
      (conso fst rst bv)
      (fresh [flipb res]
        (bit-noto fst flipb)
        (conso flipb res out)
        (bitvector-noto rst res)))]))

(defn bitvector-shl1o [bv out]
  "Turn [b1 b2 b3 b4 ... bn] into [b2 b3 b4 ... bn-1 0]."
  (fresh [fstbit rstbits]
    (bito fstbit)
    (conso fstbit rstbits bv)
    (appendo rstbits [0] out)))

(defn bitvector-shr1o
  "Turn [b1 b2 b3 b4 ... bn] into [0 b1 b2 b3 b4 ... bn-1]."
  [bv out]
  (fresh [fstbv lastbit]
    (bito lastbit)
    (appendo fstbv [lastbit] bv)
    (conso 0 fstbv out)))

(defn bitvector-shr4o
  "Turn [b1 b2 b3 b4 ... bn] into [0 0 0 0 b1 b2 b3 b4 ... bn-3]."
  [bv out]
  (fresh [tmp1 tmp2 tmp3]
    (bitvector-shr1o bv tmp1)
    (bitvector-shr1o tmp1 tmp2)
    (bitvector-shr1o tmp2 tmp3)
    (bitvector-shr1o tmp3 out)))

(defn bitvector-shr16o
  "Turn [b1 b2 b3 b4 ... bn] into [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 b1
  b2 b3 b4 ... bn-15]."
  [bv out]
  (fresh [tmp1 tmp2 tmp3]
    (bitvector-shr4o bv tmp1)
    (bitvector-shr4o tmp1 tmp2)
    (bitvector-shr4o tmp2 tmp3)
    (bitvector-shr4o tmp3 out)))

;; todo: "and" | "or" | "xor" | "plus"

(comment



)




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
