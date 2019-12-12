(define (pr1-27 n)
  (define (pr1-27-rec m)
    (carmichael-test m)
    (if (not (= m n))
       (pr1-27-rec (+ m 1)))
  )
  (pr1-27-rec 2))

(define (carmichael-test m)
  (display m)
  (display #\Space)
  (display (fermat-test m))
  (newline))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else  (remainder (* base (expmod base (- exp 1) m)) m))))

(define (remainder n m)
  (modulo n m))

(define (square n)
  (* n n))

