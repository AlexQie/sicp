(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(define (length items)
  (if (null? items)
      0
      (+ (length (cdr items)) 1)))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; excise 2.17
(define (last-pair l)
  (if (= (length l) 1)
      l
      (last-pair (cdr l))))

;; excise 2.18
(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items))
              (list (car items)))))

;; excise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values)))))

(define (no-more? items)
  (null? items))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define us-coins (list 50 10 5 1 25))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))
