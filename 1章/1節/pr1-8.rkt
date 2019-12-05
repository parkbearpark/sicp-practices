(define (pr1-8 x)
  (pr1-8-iter 1.0 x 0))

(define (pr1-8-iter guess x guess_before)
  (if (good-enough? guess guess_before)
      guess
      (pr1-8-iter (improve-cubic guess x) x guess)))

(define (improve-cubic y x)
  (/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (good-enough? guess guess_before)
  (display guess_before)
  (newline)
  (< (abs (- guess guess_before) ) 0.001))