(ns com.hackerschool.icfp-contest-2013.generate-bv
  (:require [clojure.string :as str]
            [com.hackerschool.icfp-contest-2013.lambda-bv :refer [run-program]]
            [clojure.core.match :refer [match]]))

;;TODO: memoize fns for speed.

(def all-operators
  "Every operator in the λBV language."
  '[not shl1 shr1 shr4 shr16
    plus and or xor
    if0])

(def min-size
  {'not 2 'shl1 2 'shr1 2 'shr4 2 'shr16 2
   'plus 3 'and 3 'or 3 'xor 3
   'if0 4
   'fold 5})

;; (op2 (op1 1) (op1 0)) ;; 4 left

;; (for [e0 (gen-exprs 3 ops)
;;       e1 (gen-exprs (- coutner (size e0)) ops)]
;;   [op2 e0 e1])

(defn generate-expressions-with
  "Generate all expressions possible from an operator and a set of expressions."
  [operator expressions]
  (case operator
    (shl1 shr1 shr4 shr16 not) (for [expr expressions]
                                 [operator expr])
    (plus and or xor) (for [expr1 expressions
                            expr2 expressions]
                        [operator expr1 expr2])
    (if0) (for [expr1 expressions
                expr2 expressions
                expr3 expressions]
            [operator expr1 expr2 expr3])))

(defn generate-expressions'
  "Generate all expressions as above; accumulate in expressions."
  [size operators expressions]
  (if (= size 0)
    expressions
    (let [new-expressions
          (mapcat (fn [op]
                    (generate-expressions-with op expressions))
                  operators)]
      (generate-expressions'
       (dec size)
       operators
       (concat new-expressions expressions)))))

(defn generate-expressions
  "Generate all expressions with a specified size and operator set."
  [size operators]
  (generate-expressions' size operators '[0 1 x]))

(defn generate-programs
  "Generate all programs with a specified size and operator set."
  [size operators]
  (map (fn [expr]
         ['lambda ['x] expr])
       (generate-expressions size operators)))

(comment
  (generate-programs 2 '[and])

  (take 5 (generate-programs 10 '[and if0 shr4 xor]))


  generate all programs with opset
  test against input/output pairs
  submit first program that passes

  (generate-programs 1 '[plus])

  )


(defn rand-longs []
  ;;TODO properly sample from full space of 64 bit vectors
  (repeatedly 256 #(long (* (rand) Long/MAX_VALUE))))

(defn read-expression [s]
  (-> s
      (str/replace "(" "[")
      (str/replace ")" "]")
      read-string))

(defn correct-program?
  "Returns true if the program produces the correct output given a map of {input output} examples"
  [program examples]
  (every? (fn [[in out]]
            (= (run-program program in) out))
          examples))

(defn find-solution
  "Repeatedly generate new I/O examples and run all candidates against them
   until at most one program is left standing. Return the winner or the empty
   vector, if no program won."
  [candidates oracle]
  (if (<= (count candidates) 1)
    candidates
    (let [examples (oracle)
          remaining-candidates (filter (fn [candidate]
                                         (correct-program? candidate examples))
                                       candidates)]
      (recur remaining-candidates oracle))))

(defn training-oracle
  "Return an I/O oracle given a training problem."
  [training-problem]
  (fn []
    (let [e (read-expression (:challenge training-problem))]
      (into {} (for [l (rand-longs)]
                 [l (run-program e l)])))))

(defn unchecked-long
  [bi]
  (if (= (class bi) clojure.lang.BigInt)
    (.longValue (.bipart bi))
    (long bi)))

(defn hexstr
  "given a number, returns an 8-byte hex string of the form '0x0000000000000000'"
  [n]
  (let [unpadded-str (Long/toHexString n)
        cnt (count unpadded-str)]
    (str "0x"
         (apply str (repeat (- 16 cnt) "0"))
         (str/upper-case unpadded-str))))

(defn rand-64-bit-hex []
  (map hexstr (rand-longs)))

(comment
  (def test-problem
    {:challenge "(lambda (x_23265) (plus (or (or (or (if0 (plus (xor (not x_23265) 0) 0) x_23265 x_23265) x_23265) x_23265) x_23265) x_23265))",
     :size 18,
     :operators ["if0" "not" "or" "plus" "xor"],
     :id "h7oZTxtwiD00OapNDtCAstRG"})

  (let [e (read-expression "(lambda (x_23265) (plus (or (or (or (if0 (plus (xor (not x_23265) 0) 0) x_23265 x_23265) x_23265) x_23265) x_23265) x_23265))",)]
    (into {} (for [l (rand-longs)]
               [l (run-program e l)])))

  ;; TODO: this has been processed slightly:
  ;; operators is normally a string-vector
  (def small-problem
    {:challenge "(lambda (x) (shr4 (shl1 x)))",
     :size 3,
     :operators ['shl1 'shr4],
     :id "foo$to-the$bar"})

  (find-solution (generate-programs
                  (:size small-problem)
                  (:operators small-problem))
                 (training-oracle small-problem))

  (unchecked-long 0xFFFFFFFFFFFFFFFF)

  265 random longs
  (rand-long Long/MAX_VALUE)


  Long
  (- (rand) 0.5)

  )

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
    [['lambda _ e]] (+ 1 (size e))
    [(:or 0 1)] 1
    [(_ :guard symbol?)] 1
    [['if0 e0 e1 e2]] (+ 1 (size e0) (size e1) (size e2))
    [['fold e0 e1 ['lambda _ e2]]] (+ 2 (size e0) (size e1) (size e2))
    [[op1 e]] (+ 1 (size e))
    [[op2 e0 e1]] (+ 1 (size e0) (size e1))))
