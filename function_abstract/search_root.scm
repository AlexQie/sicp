(define (search f negative-point positive-point)
  (let ((midpoint (average negative-point positive-point)))
    (if (close-enough? negative-point positive-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f negative-point midpoint))
                ((negative? test-value)
                 (search f midpoint positive-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y) 2.0))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (positive? a-value) (negative? b-value))
           (search f b a))
          ((and (positive? b-value) (negative? a-value))
           (search f a b))
          (else
           (error "Values are not of opposite sign")))))

(define tolerance 0.00001)

(define (fixed-point f fitst-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try fitst-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;excise 1.35
(define (golden-ratio)
  (fixed-point (lambda (y) (average y (+ (/ 1 y) 1.0))) 1.0))

;excise 1.36
(define (x-x)
  (fixed-point (lambda (x) (/ (log 1000.0) (log x))) 2.0))

(define (x-x-average)
  (fixed-point (lambda (x) (average x (/ (log 1000.0) (log x)))) 2.0))

;excise 1.37
(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

; excise 1.38
(define (approximate-e iter-num)
  (define (e-d i)
    (if (= (remainder (+ i 1) 3) 0)
        (* 2 (/ (+ i 1) 3))
        1))
  (+ 2 (cont-frac (lambda (x) 1.0) e-d iter-num)))

;excise 1.39
(define (tan-cf x k)
  (define (tan-n i)
    (if (= i 1)
        x
        (* (square x) -1)))
  (cont-frac tan-n (lambda (i) (- (* i 2) 1)) k))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) x))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) x))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x)) 1.0))

;exicise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* (square x) a)
                 (* x b)
                 c)))

;excise 1.41
(define (double g)
  (lambda (x) (g (g x))))

;excise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;excise 1.43
(define (repeat f n)
  (cond ((= n 1) f)
        ((= (remainder n 2) 0) (repeat (compose f f) (/ n 2)))
        (else (compose f (repeat (compose f f) (/ n 2))))))

;excise 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (+ x dx)) (f x) (f (- x dx)))
                 3)))

(define (smooth-n f n)
  (repeat (smooth f) n))
