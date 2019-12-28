(define (newtons-method a b c)
  (fixed-point (cubic a b c) 1))

(define (cubic a b c)
  (lambda (x) (+ (expt x 3) (* a (expt x 2)) (* b x) c)))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
   (if (> v1 10)
       (v1))
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))