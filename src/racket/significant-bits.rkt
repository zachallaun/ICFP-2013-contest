#lang racket

(define (driver expr)
  (~r (sb* expr) #:base 16 #:min-width 16 #:pad-string "0"))

(define *all-fs* #xFFFFFFFFFFFFFFFF)

(define (sb* expr)
  (let* [(bits *all-fs*)
         (rp   0)
         (shl (lambda (steps)
                (set! rp (- rp steps))
                (when (< rp 0)
                    (set! bits (bitwise-and
                                bits
                                (shift
                                 *all-fs* (- 0 rp)))))))
         (shr (lambda (steps)
                (set! rp (+ rp steps))
                (when (> rp 0)
                    (set! bits (bitwise-and
                                bits
                                (shift)
                                 *all-fs* rp)))))]
    (define (loop expr)
      (match expr
        [`(lambda (,_) ,body)
         (loop body)]
        [`(not ,e)
         (loop e)]
        [`(shl1 ,e)
         (loop e)
         (shl 1)
         bits]
        [`(shr1 ,e)
         (loop e)
         (shr 1)
         bits]
        [`(shr4 ,e)
         (loop e)
         (shr 4)
         bits]
        [`(shr16 ,e)
         (loop e)
         (shr 16)
         bits]
        [(or `(and ,(? integer? const) ,e)
             `(and ,e ,(? integer? const)))
         (when (and (>= rp 0) (not const))
             (set! bits (bitwise-and bits (bitwise-not (shift 1 (- 0 rp))))))
         (loop e)]
        [(or `(or ,(? integer? const) ,e)
             `(or ,e ,(? integer? const)))
         (when (and (>= rp 0) const)
             (set! bits (bitwise-and bits (bitwise-not (shift 1 (- 0 rp))))))
         (loop e)]
        [`(,_ ,e1 ,e2) ;; any binary op
         (bitwise-ior
          (sb* e1)
          (sb* e2))]
        [(? symbol?) bits]
        [(? integer?) bits]))
    (loop expr)))

(define (shift v n)
  (bitwise-and (arithmetic-shift v n)  #xFFFFFFFFFFFFFFFF))
