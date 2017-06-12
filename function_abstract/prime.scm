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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime_test n (real-time-clock)))

(define (start-prime_test n start-time)
  (if (prime? n)
      (report-time (- (real-time-clock) start-time))))

(define (report-time elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start n)
  (if (> n 1)
      (cond (odd? start) (timed-prime-test start)
            (even? start) (timed-prime-test (+ start 1)))))
