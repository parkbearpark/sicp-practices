(define (halve n)
  (/ n 2))

(define (double n)
  (+ n n))

(define (pr1-18-iter n m cnt res)
  (cond ((= cnt 1) res)
        ((and (even? cnt) (< (double res) m)) (pr1-18-iter n m (halve cnt) (double res)))
        (else (pr1-18-iter n m (- cnt 1) (+ res n)))))

(define (pr1-18 n m)
  (pr1-17-iter n m m n))