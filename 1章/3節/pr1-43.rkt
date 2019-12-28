(define (repeated f n)
  (define (repeated-iter res-f cnt)
    (if (= cnt n)
        res-f
        (repeated-iter (lambda (x) (f (res-f x))) (+ cnt 1))))
  (repeated-iter f 1))

(define (square x)
  (* x x))