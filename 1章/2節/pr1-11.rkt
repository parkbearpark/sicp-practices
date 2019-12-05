(define (pr1-11-rec n)
  (if (< n 3)
      n
      (+ (1-1-rec (- n 1)) (* 2 (1-1-rec (- n 2))) (* 3 (1-1-rec (- n 3))))))

(define (pr1-11-iter n)
  (define (1-1-iter-do m val1 val2 val3)
     (if (= m n)
        (+ val1 val2 val3)
        (1-1-iter-do (+ m 1) (+ val1 (* 2 val2) (* 3 val3)) (* 2 val1) (* 3 val2))))
  (1-1-iter-do 3 2 1 0))