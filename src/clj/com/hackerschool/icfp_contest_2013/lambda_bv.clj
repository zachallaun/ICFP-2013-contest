(ns com.hackerschool.icfp-contest-2013.lambda-bv
  (:require [clojure.core.match :refer [match]]))

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

(defn run
  "Runs a λBV sexp given an argument."
  [e arg]
  {:pre  [(long? arg)]
   :post [long?]}
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
      [(['lambda ([id] :seq) body] :seq)]
      (run-body body {id arg}))))




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
