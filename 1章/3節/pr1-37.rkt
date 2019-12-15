(define (pr1-37 k)
  (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))

(define (cont-frac n d k)
  (define (cont-frac-iter i res)
    (if (= i 0)
        res
        (cont-frac-iter (- i 1) (/ (n i) (+ res (d i))))))
  (cont-frac-iter k 0))

(define (linear-cont-frac n d k)
  (if (= k 0)
      0
      (/ (n k) (+ (d k) (linear-cont-frac n d (- k 1))))))