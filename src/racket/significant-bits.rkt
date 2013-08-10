#lang racket

(define (hex expr)
  (~r expr #:base 16 #:min-width 16 #:pad-string "0"))

(define (driver expr)
  (hex (sb* expr)))

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
                                 *all-fs* rp))))))
         (shr (lambda (steps)
                (set! rp (+ rp steps))
                (when (> rp 0)
                    (set! bits (bitwise-and
                                bits
                                (shift
                                 *all-fs* rp))))))]
    (define (loop expr)
      (match expr
        [`(fold ,_ ,_) bits]
        [`(lambda (,_) ,body)
         (loop body)]
        [`(not ,e)
         (loop e)
         bits]
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
         (loop e)
         (when (and (>= rp 0) (= const 0))
             (set! bits (bitwise-and bits (bitwise-and *all-fs*
                                          (bitwise-not (shift 1 rp))))))
         bits]
        [(or `(or ,(? integer? const) ,e)
             `(or ,e ,(? integer? const)))
         (loop e)
         (when (and (>= rp 0) (= const 1))
             (set! bits (bitwise-and bits (bitwise-and *all-fs* 
                                          (bitwise-not (shift 1 rp))))))
         bits]
        [(or `(,_ ,(? integer?) ,e)
             `(,_ ,e ,(? integer?)))
         (loop e)
         bits]
         
        [`(,_ ,e1 ,e2) ;; any binary op
         (set! bits (bitwise-ior
          (sb* e1)
          (sb* e2)))
          bits]
        [(? symbol?) bits]
        [(? integer?) bits]))
    (loop expr)))

(define (shift v n)
  (bitwise-and (arithmetic-shift v n)  #xFFFFFFFFFFFFFFFF))
