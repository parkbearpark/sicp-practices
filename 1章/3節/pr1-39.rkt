(define (tan-cf x k)
  (exact->inexact (cont-frac (lambda (i) (* i i)) (lambda (i) (+ 1 (* 2 i))) k x)))

(define (cont-frac n d k x)
  (define (cont-frac-iter i)
    (if (= i k)
        (/ x (d i))
        (if (= i 0)
            (/ x (- (d i) (cont-frac-iter (+ i 1))))
            (/ (n x) (- (d i) (cont-frac-iter (+ i 1)))))
    ))
  (cont-frac-iter 0))