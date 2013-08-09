(ns com.hackerschool.icfp-contest-2013.t-lambda-bv
  (:require [midje.sweet :refer :all]
            [com.hackerschool.icfp-contest-2013.lambda-bv :refer :all]))

(fact "fold"
  (fold 0 1 (fn [acc x] (bit-shift-left acc 1))) => 256
  (fold 0x1122334455667788 [] (fn [acc x] (conj acc x)))
    => [0x88 0x77 0x66 0x55 0x44 0x33 0x22 0x11])

(facts "about running λBV code"

  (fact "can be a constant"
    (run '(lambda (x) 1) 123) => 1
    (run '(lambda (x) 0) 123) => 0)

  (fact "can be the argument"
    (run '(lambda (x) x) 123) => 123)

  (fact "can be a unary operation: not, shl1, shr1, shr4, shr16"
    (run '(lambda (x) (not x)) 0) => -1

    (run '(lambda (x) (shl1 x)) 1) => 2
    (run '(lambda (x) (shl1 x)) -1) => #(> 0 %)

    (run '(lambda (x) (shr1 x)) 2) => 1

    (run '(lambda (x) (shr4 x)) 16) => 1

    (run '(lambda (x) (shr16 x)) 65536) => 1
    (run '(lambda (x) (shr16 x)) 0) => 0)

  (fact "can be a binary operation: and or xor plus"
    (run '(lambda (x) (and x 1)) 2) => 0

    (run '(lambda (x) (or x 1)) 2) => 3
    (run '(lambda (x) (or x 1)) 1) => 1

    (run '(lambda (x) (xor x 1)) 2) => 3
    (run '(lambda (x) (xor x 1)) 1) => 0

    (run '(lambda (x) (plus x 1)) 1) => 2
    (run '(lambda (x) (plus x 0)) 8) => 8)

  (fact "can be arbitrarily nested"
    (run '(lambda (x) (plus (shr1 x) (shl1 1))) 2) => 3)

  (fact "can include fold"
    (run '(lambda (x) (fold x 0 (lambda (y z) (or y z)))) 0x1122334455667788)
      => 255

    (run '(lambda (x) (fold x 0 (lambda (y z) (plus y z)))) 0x1122334455667788)
      => 612)

  (fact "can include if"
    (run '(lambda (x) (if0 x 0 1)) 1) => 1
    (run '(lambda (x) (if0 x 0 1)) 42) => 1
    (run '(lambda (x) (if0 x 0 1)) 0) => 0
    (run '(lambda (x) (if0 x 0 1)) -1) => 1

    (run '(lambda (x) (if0 x
                        (plus x 1)
                        (shl1 x)))
      0) => 1

    (run '(lambda (x) (if0 x
                        (plus x 1)
                        (shl1 x)))
      1) => 2))
