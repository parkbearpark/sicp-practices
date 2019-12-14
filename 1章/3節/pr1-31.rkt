(define (product term n next m)
  (define (product-iter a res)
    (if (> a m)
        res
        (product-iter (next a) (* res (term a)))))
  (product-iter n 1))

(define (linear-product term n next m)
  (if (> n m)
      1
      (* (term n) (linear-product term (next n) next m))))

(define (id x) x)
(define (inc x) (+ x 1))

(define (step-by-2 x) (+ x 2))
(define (square x) (* x x))
(define (nume-product x) (* x (+ x 2)))

(define (factorial n m)
  (product x1 n inc m))

(define (pi)
  (exact->inexact (* 4 (/ (product nume-product 2 step-by-2 10000) (product square 3 step-by-2 10001)))))