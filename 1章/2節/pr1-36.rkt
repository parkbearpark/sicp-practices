(define (pr1-36)
  (fixed-point pr1-36-func 2))

(define (pr1-36-ave)
  (fixed-point (lambda (x) (/ (+ x (pr1-36-func x)) 2)) 2))

(define (pr1-36-func x)
  (/ (log 1000) (log x)))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
