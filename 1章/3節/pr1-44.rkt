(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
        (f x)
        (f (+ x dx)))
     3)))

(define (square x)
  (* x x))

(define (repeated f n)
  (define (repeated-iter res-f cnt)
    (if (= cnt n)
        res-f
        (repeated-iter (lambda (x) (f (res-f x))) (+ cnt 1))))
  (repeated-iter f 1))

(define (pr1-44 f n)
  (repeated (smooth f) n))