(define (pr1-38 k)
  (+ 2(cont-frac (lambda (i) 1.0)
             get-d
             k)))

(define (cont-frac n d k)
  (define (cont-frac-iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (cont-frac-iter (+ i 1)) (d i))))
    )
  (cont-frac-iter 0))

(define (get-d i)
  (let ((m (modulo i 3)) (div (quotient  i 3)))
  (if (= m 1)
      (* 2 (+ div 1))
      1)))