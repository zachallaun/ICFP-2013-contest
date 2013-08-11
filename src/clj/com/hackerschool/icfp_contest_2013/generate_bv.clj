(ns com.hackerschool.icfp-contest-2013.generate-bv
  (:require [clojure.string :as str]
            [com.hackerschool.icfp-contest-2013.lambda-bv :refer [run-program]]))

;;TODO: memoize fns for speed.

(def all-operators
  "Every operator in the λBV language."
  '[not shl1 shr1 shr4 shr16
    and or xor
    if0])

(defn generate-expressions-with
  "Generate all expressions possible from an operator and a set of expressions."
  [operator expressions]
  (case operator
    (shl1 shr1 shr4 shr16 not)  (for [expr expressions]
                                  [operator expr])
    (and or xor)                (for [expr1 expressions
                                      expr2 expressions]
                                  [operator expr1 expr2])
    (if0)                       (for [expr1 expressions
                                      expr2 expressions
                                      expr3 expressions]
                                  ['if0 expr1 expr2 expr3])))

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
  (generate-programs 1 '[and shl1 shr4 xor or])
  (take 50 (generate-programs 10 '[and if0 shr4 xor]))

  generate all programs with opset
  test against input/output pairs
  submit first program that passes


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
  (def test
    {:challenge "(lambda (x_23265) (plus (or (or (or (if0 (plus (xor (not x_23265) 0) 0) x_23265 x_23265) x_23265) x_23265) x_23265) x_23265))",
     :size 18,
     :operators ["if0" "not" "or" "plus" "xor"],
     :id "h7oZTxtwiD00OapNDtCAstRG"})

  (let [e (read-expression "(lambda (x_23265) (plus (or (or (or (if0 (plus (xor (not x_23265) 0) 0) x_23265 x_23265) x_23265) x_23265) x_23265) x_23265))",)]
    (into {} (for [l (rand-longs)]
               [l (run-program e l)])))

  (unchecked-long 0xFFFFFFFFFFFFFFFF)

  265 random longs
  (rand-long Long/MAX_VALUE)



  Long
  (- (rand) 0.5)


  )
