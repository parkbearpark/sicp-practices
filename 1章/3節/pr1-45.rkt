(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average a b)
  (/ (+ a b) 2))

(define (repeated f n)
  (define (repeated-iter res-f cnt)
    (if (= cnt n)
        res-f
        (repeated-iter (lambda (x) (f (res-f x))) (+ cnt 1))))
  (repeated-iter f 1))

(define (square-root x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (triangle-root x)
  (fixed-point (average-damp (lambda (y) (/ x (expt y 2)))) 1.0))

(define (fourth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 3)))) 1.0))

(define (nth-root x n m)
  (fixed-point ((repeated average-damp m) (lambda (y) (/ x (expt y (- n 1))))) 1.0))