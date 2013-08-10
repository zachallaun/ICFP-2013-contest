(ns com.hackerschool.icfp-contest-2013.generate-bv)

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
)
