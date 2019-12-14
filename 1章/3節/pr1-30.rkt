(define (sum term a next b)
  (define (iter a result)
    (if (= a b)
        (+ a result)
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (x1 x) x)
(define (inc n) (+ n 1))