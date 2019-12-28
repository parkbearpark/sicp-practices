(define (iterative-improve good-enough? improve n)
  (define (iter-func val)
    (if (good-enough? val n)
        val
        (iter-func (improve val n))))
  (iter-func 1.0))

(define (sqrt-good-enough? guess x)
  (< (abs (- (expt guess 2) x)) 0.00001))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (iterative-improve2 good-enough? improve f)
  (define (iter-func val)
    (let ((next (improve f val)))
      (if (good-enough? val next)
        val
        (iter-func next))))
  (iter-func 1.0))

(define (fixed-good-enough? v1 v2)
  (< (abs (- v1 v2)) 0.00001))

(define (fixed-improve f x)
  (f x))