(define (filtered-accumlate filter combiner null-value term a next b)
  (define (accumlate-iter n res)
    (if (> n b)
        res
        (if (filter n)
         (accumlate-iter (next n) (combiner (term n) res))
         (accumlate-iter (next n) res)
         )))
  (accumlate-iter a null-value))

(define (filtered-accumlate-b filter combiner null-value term a next b)
  (define (accumlate-iter n res)
    (if (> n b)
        res
        (if (filter n (+ b 1))
         (accumlate-iter (next n) (combiner (term n) res))
         (accumlate-iter (next n) res)
         )))
  (accumlate-iter a null-value))

(define (pr1-33-a a b)
  (filtered-accumlate prime? + 0 expt-2 a inc b))

(define (pr1-33-b n)
  (filtered-accumlate-b disjoint? * 1 id 1 inc (- n 1)))

(define (expt-2 n) (expt n 2))
(define (inc n) (+ n 1))

(define (id n) n)
(define (disjoint? a b)
  (if (> a b)
      (if (= (modulo a b) 0)
          #f
          #t)
      (if (= (modulo b a) 0)
          #f
          #t)))

(define (prime? n)
  (if (= n 1)
      #f
      (pr1-28 n 100)
      ))
      
(define (pr1-28 n times)
  (define (pr1-28-rec time)
    (cond ((= time 0) #t)
          ((miller_rabin-test n) (pr1-28-rec (- time 1)))
          (else #f)))
  (pr1-28-rec times))
        

(define (miller_rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1)
  )
  (try-it (+ (random (- n 1)) 1)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
         ((and (not (= base 1)) (not (= base (- m 1))) (= (remainder (square base) m) 1)) 0)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (remainder n m)
  (modulo n m))

(define (square n)
  (* n n))

