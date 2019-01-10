#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define fast-expt-iter

(define (fast-expt-iter b n)
  ; Your code here
  (error "Not yet implemented")
)

; Exericse 2 - Define phi

(define (phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Exercise 3 - Define cont-frac

;; Recursive version
(define (cont-frac n d k)
  (define (recurs i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recurs (+ i 1))))))
  (recurs 1))

;; Iterative version
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (e k)
  (+ 2 (cont-frac (lambda (i) (identity 1.0))
                  (lambda (i) (if (= (remainder (+ i 1) 3) 0)
                                  (- i (quotient i 3))
                                  1))
                  k)))

; Exercise 4 - Define next-perf

(define (next-perf n)
  ;  Your code here
  (error "Not yet implemented")
)

; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

Your explanation here

|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|

Formula for expt:

Formula for expt-iter:

|#
