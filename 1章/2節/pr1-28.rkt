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

