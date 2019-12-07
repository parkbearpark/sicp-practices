(define (fast-expt-iter n m cnt res)
  (cond ((= cnt m) res)
        ((<= (* 2 cnt) m) (fast-expt-iter n m (* cnt 2) (* res res)))
        (else (* res (fast-expt-iter n (- m cnt) 1 n)))))

(define (pr1-16 n m)
  (fast-expt-iter n m 1 n))