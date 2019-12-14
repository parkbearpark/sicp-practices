(define (accumlate combiner null-value term a next b)
  (define (accumlate-iter n res)
    (if (> n b)
        res
        (accumlate-iter (next n) (combiner (term n) res))))
  (accumlate-iter a null-value)
  )

(define (linear-accumlate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (linear-accumlate combiner null-value term (next a) next b))))

(define (sum a b)
  (accumlate + 0 id a inc b))

(define (product a b)
  (accumlate * 1 id a inc b))

(define (id x) x)
(define (inc x) (+ x 1))