(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (cube a) (* a a a))

(define (sum-cube a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term a)
    (/ 1.0 (* a (+ a 2))))
  (define (pi-next a)
    (+ a 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

; excise 1.29 Simpson Rule
(define (coef-sum coef term a next b i)
  (if (> a b)
      0
      (+ (* (coef i) (term a))
         (coef-sum coef term (next a) next b (inc i)))))

(define (simpson-rule f a b n)
  (define (simpson-coef i)
    (cond ((or (= i 0) (= i n)) 1)
          ((odd? i) 4)
          (else 2)))
  (define (h)
    (/ (- b a) n))
  (define (simpson-next x)
    (+ (h) x))
  (* (coef-sum simpson-coef f a simpson-next b 0) (/ (h) 3.0)))

; excise 1.30
(define (iter-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; excise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-pi upper)
  (define (product-pi-next x) (+ x 2))
  (* 4 (/ (* 2
             upper
             (product square 4.0 product-pi-next (- upper 1)))
          (product square 3.0 product-pi-next upper))))

(define (factorial n)
  (define (f-term x) x)
  (if (= n 0) 1
      (product f-term 1 inc n)))

; iter-version product
(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

; excise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (acc-sum term a next b)
  (define (add x y) (+ x y))
  (accumulate add 0 term a next b))

(define (acc-product term a next b)
  (define (mul x y) (* x y))
  (accumulate mul 1 term a next b))

;excise 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filtered-accumulate
                               combiner null-value term (next a) next b filter)))
        (else (combiner null-value (filtered-accumulate
                                    combiner null-value term (next a) next b filter)))))

(define (filtered-sum term a next b filter)
  (define (add x y) (+ x y))
  (filtered-accumulate add 0 term a next b filter))

(define (filtered-product term a next b filter)
  (define (mul x y) (* x y))
  (filtered-accumulate mul 1 term a next b filter))

; sum of prime between [a, b]
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder a b) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (sum-prime a b)
  (define (sp-term x) x)
  (filtered-sum sp-term a inc b prime?))

; product of all primes smaller than n
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prime-product n)
  (define (pp-filter x) (= (gcd n x) 1))
  (define (sp-term x) x)
  (filtered-product sp-term 2 inc n pp-filter))
