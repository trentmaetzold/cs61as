#lang sicp

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (identity x)
  x)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Exercise 1.29
(define (simpson f n a b)
  (define h (/ (- b a) n))
  (define (recurs k)
    (define y_k (f (+ a (* k h))))
    (cond ((= k n) y_k)
          ((= k 0) (+ y_k (recurs (+ k 1))))
          ((odd? k) (+ (* 4 y_k) (recurs (+ k 1))))
          ((even? k) (+ (* 2 y_k) (recurs (+ k 1))))))
  (* (/ h 3) (recurs 0)))

;; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; Exercise 1.31b
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;; Exercise 1.32a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

;; Exercise 1.32b
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-acc-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-acc-iter term a next b)
  (accumulate-iter * 1 term a next b))

;; Exercise 1.33
(define (filtered-accumulate combiner null-value filter? term a next b)
  (cond ((> a b) null-value)
        ((filter? a) (combiner (term a)
                               (filtered-accumulate combiner null-value filter? term (next a) next b)))
        (else (filtered-accumulate combiner null-value filter? term (next a) next b))))

;; Excercise 1.33a
(define (sos-prime a b)
  (define (prime? num)
    #t)
  (filtered-accumulate + 0 prime? square a inc b))

;; Exercise 1.33b
(define (product-pos-int-rel-prime n)
  (define (rel-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 rel-prime? identity 1 inc n))

;; Exercise 1.34
#|
(define (f g)
  (g 2))

(f f)
(f 2)
(2 2); error, 2 is a literal, not a procedure
|#
