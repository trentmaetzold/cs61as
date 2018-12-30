#lang sicp

#| ----- Section 1.3.1 ----- |#

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (identity x)
  x)

(define (average x y)
  (/ (+ x y) 2))

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

#| ----- Section 1.3.2 ----- |#

;; Exercise 1.34
#|
(define (f g)
  (g 2))

(f f)
(f 2)
(2 2); error, 2 is a literal, not a procedure
|#

#| ----- Section 1.3.3 ----- |#

(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-fp x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; Exercise 1.35
(define golden-ratio-fp
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

;; Exercise 1.36    (Come back after finishing 1.2.2)

;; Exercise 1.37a
(define (cont-frac n d k)
  (define (cont-frac-rc i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-frac-rc (+ i 1))))))
  (cont-frac-rc 1))

(define (cont-frac-test k)
  (cont-frac (lambda (j) 1.0)
             (lambda (j) 1.0)
             k))

;; i = 12, accurate to 4 decimal places

;; Exercise 1.37b    (Come back after finishing 1.2.2)

;; Exercise 1.38
(define euler-cf
  (+ 2.0 (cont-frac (lambda (j) 1.0)
                    (lambda (j)
                      (if (= (remainder (+ j 1) 3) 0)
                          (- j (quotient j 3))
                          1.0))
                    10)))

;; Exercise 1.39
(define (tan-cf x k)
  (cont-frac (lambda (j)
               (if (= j 1)
                   x
                   (- (square x))))
             (lambda (j)
               (- (* j 2.0) 1.0))
             k))

#| ------ Section 1.3.4 ----- |#

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-ad x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-nm x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fpt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt-fpt-nm x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;; Exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

;; Exercise 1.41
(define (double g)
  (lambda (x) (g (g x))))

#|
(((double (double double)) inc) 5)
((double double) ((double double) inc) 5)
((double double) ((double (double inc)))) 5)
((double double) ((double (inc (inc x)))) 5)
((double double) (((inc (inc (inc (inc x)))))) 5)
(double (double (inc (inc  (inc (inc x))))) 5)
(double (inc (inc (inc (inc (inc (inc (inc (inc x)))))))) 5)
(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))
21
|#

;; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; Exercise 1.43
(define (repeated f n)
  (define (recurs i)
    (cond ((= n 1) f)
          ((= i (- n 1)) (compose f f))
          (else (compose f (recurs (+ i 1))))))
  (recurs 1))

;; Exercise 1.44
(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

;; ((repeated smooth n) f)

;; Exercise 1.45
;; The tests I've done work, but the solution is quite a bit different than
;; others I've seen online. The procedure (expt x n) is built-in to R5RS.

(define (root n x)
  (fixed-point ((repeated average-damp (if (< n 4)
                                           1
                                           (- n 2)))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

;; Exercise 1.46

(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (if (good-enough? guess)
          guess
          (try (improve guess))))
    (try first-guess)))

(define (sqrt-ii x)
  ((iterative-improve (lambda (y)
                        (< (abs (- (square y) x)) 0.001))
                      (lambda (y)
                        (average y (/ x y))))
   1.0))

(define (fixed-point-ii f first-guess)
  ((iterative-improve (lambda (x)
                        (< (abs (- x (f x))) 0.00001))
                      f)
   first-guess))
