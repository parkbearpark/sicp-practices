(define (halve n)
  (/ n 2))

(define (double n)
  (+ n n))

(define (pr1-17-rec n m cnt)
  (cond ((= cnt 0) 0)
        ((even? cnt) (double (pr1-17-rec n m (halve cnt))))
        (else (+ n (pr1-17-rec n m (- cnt 1))))))

(define (pr1-17 n m)
  (pr1-17-rec n m m))