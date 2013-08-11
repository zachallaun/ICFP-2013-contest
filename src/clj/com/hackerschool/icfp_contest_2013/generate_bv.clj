(ns com.hackerschool.icfp-contest-2013.generate-bv
  (:require [clojure.string :as str]
            [com.hackerschool.icfp-contest-2013.lambda-bv :refer [run-program]]
            [clojure.core.match :refer [match]]))

;;TODO: memoize fns for speed.

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

(def all-operators
  "Every operator in the λBV language."
  '[not shl1 shr1 shr4 shr16
    plus and or xor
    if0])

(defn eliminate-dead-code
  [expr]
  (match [expr]

         [['lambda [x] body]] ['lambda [x] (eliminate-dead-code body)]

         [[(:or 'shr1 'shr4 'shr16) 0]] 0

         [['plus e1 0]] (eliminate-dead-code e1)
         [['plus 0 e1]] (eliminate-dead-code e1)

         [['or e1 0]] (eliminate-dead-code e1)
         [['or 0 e1]] (eliminate-dead-code e1)

         [['xor e1 0]] (eliminate-dead-code e1)
         [['xor 0 e1]] (eliminate-dead-code e1)

         [['and e1 0]] 0
         [['and 0 e1]] 0

         [['if0 0 e1 _]] (eliminate-dead-code e1)
         [['if0 1 _ e2]] (eliminate-dead-code e2)

         [[unop e1]] [unop (eliminate-dead-code e1)]

         [[binop e1 e2]] [binop
                          (eliminate-dead-code e1)
                          (eliminate-dead-code e2)]

         [['if0 e0 e1 e2]] ['if0
                            (eliminate-dead-code e0)
                            (eliminate-dead-code e1)
                            (eliminate-dead-code e2)]

         [_] expr))

(defn constant-fold
  [expr]
  (match [expr]
         [(:or 0 1)] expr

         [['lambda [x] body]] ['lambda [x] (constant-fold body)]

         ;; Note: Special-casing plus because constant folding it could
         ;; introduce the illegal constant 2.
         [['plus e1 e2]] expr

         [[unop e1]] expr

         [[binop e1 e2]] (let [e1' (constant-fold e1)
                               e2' (constant-fold e2)]
                           (if (and (number? e1')
                                    (number? e2'))
                             (case binop
                               and  (bit-and e1' e2')
                               xor  (bit-xor e1' e2')
                               or   (bit-or e1' e2'))
                             [binop e1 e2]))

         [['if0 e0 e1 e2]] (let [e0' (constant-fold e0)
                                 e1' (constant-fold e1)
                                 e2' (constant-fold e2)]
                             ['if0 e0' e1' e2'])

         [_] expr))

(def optimize (comp eliminate-dead-code constant-fold))

(def min-size
  {'not 2 'shl1 2 'shr1 2 'shr4 2 'shr16 2
   'plus 3 'and 3 'or 3 'xor 3
   'if0 4
   'fold 5})

(defn affordable-ops
  "Returns a subset of `ops` that could be used in expressions <= progsize"
  [ops progsize]
  (filter (fn [op]
            (<= (min-size op) progsize))
          ops))

(declare generate-exprs)

(defn generate-exprs-with
  "Generate all expressions using op less than size `progsize`"
  [op progsize ops]
  (case op
    (not shl1 shr1 shr4 shr16)
    (for [e0 (generate-exprs (dec progsize) ops)]
      [op e0])

    (plus and or xor)
    (for [e0 (generate-exprs (dec progsize) ops)
          e1 (generate-exprs (- progsize 1 (size e0)) ops)]
      [op e0 e1])

    (if0)
    (for [e0 (generate-exprs (dec progsize) ops)
          e1 (generate-exprs (- progsize 1 (size e0)) ops)
          e2 (generate-exprs (- progsize 1 (size e1)) ops)]
      [op e0 e1 e2])))

(defn generate-exprs
  "Generate all expressions with a certain size and op set."
  [progsize ops]
  (let [ops (shuffle ops)]
    (if (<= progsize 1)
      '[0 1 x]
      (let [exprs (mapcat #(generate-exprs-with % progsize ops)
                          (affordable-ops ops progsize))]
        (concat (generate-exprs (dec progsize) ops) exprs)))))

(defn generate-programs
  "Generate all programs with a certain op set and less than size `progsize`"
  [progsize ops]
  (->> (generate-exprs (dec progsize) ops)
       (map (fn [expr] ['lambda ['x] expr])) ;; make a program
       (map constant-fold)
       (map eliminate-dead-code)
       distinct))

(comment
  generate all programs with opset
  test against input/output pairs
  submit first program that passes

  (generate-programs 1 '[plus])

  (def problem (first (drop 100 (generate-programs 5 '[and or plus if0]))))
  problem
  [problem (eliminate-dead-code problem)]
  )


(defn rand-longs []
  ;;TODO properly sample from full space of 64 bit vectors
  (repeatedly #(long (* (rand) Long/MAX_VALUE))))

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
  [candidates oracle]
  (loop [examples (oracle :examples)
         culled (filter #(correct-program? % examples) candidates)]
    (when-let [attempt (first culled)]
      (let [result (oracle :submit attempt)]
        (match [result]
          [[:win]] attempt
          [[:mismatch in out]] (recur (merge {in out} (oracle :examples))
                                      (rest culled))
          [[:error msg]] (do (println (str "ERROR: " msg))
                             (recur (oracle :examples) culled)))))))

(defn training-oracle
  "Returns a function that can do two things:

   * if passed `:examples`, returns a dictionary of input-output
     examples.

   * if passed `:submit` and a guessed program, returns one of:

      * `[:win]`, if the guess is correct;

      * `[:mismatch in out]`, where `in` and `out` are a counterexample
        input and output, if the guess was incorrect;

      * `[:error msg]`, if something went wrong."
  [training-problem]
  (let [e (read-expression (:challenge training-problem))]
    (fn [& args]
      (match (vec args)
        [:examples] (into {} (for [l (take 256 (rand-longs))]
                               [l (run-program e l)]))
        [:submit attempt] (do (println (str "ATTEMPT: " attempt))
                              (if-let [[in out]
                                       (->> (take 10000 (rand-longs))
                                            (map (juxt identity #(run-program e %)))
                                            (filter (fn [[in out]]
                                                      (not= out (run-program attempt in))))
                                            (first))]
                                [:mismatch in out]
                                [:win]))))))

(comment
  (def test-problem
    {:challenge "(lambda (x_23265) (plus (or (or (or (if0 (plus (xor (not x_23265) 0) 0) x_23265 x_23265) x_23265) x_23265) x_23265) x_23265))",
     :size 18,
     :operators ["if0" "not" "or" "plus" "xor"],
     :id "h7oZTxtwiD00OapNDtCAstRG"})

  (let [small-problem
        {:challenge "(lambda (x) (shr4 (shl1 x)))",
         :size 3,
         :operators ['shl1 'shr4],
         :id "foo$to-the$bar"}]
    (find-solution (generate-programs 2 '[shl1 shr4])
                   (training-oracle small-problem)))

  (count (generate-programs 7 5 '[and shr16]))



  )
