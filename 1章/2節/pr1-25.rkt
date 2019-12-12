(define (timed-prime-test n)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 4)
      (report-prime (- (runtime) start-time))
      #f
      ))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
   
(define (remainder n m)
  (modulo n m))

(define (square n)
  (* n n))

(define (fast-expt-iter n m cnt res)
  (cond ((= cnt m) res)
        ((<= (* 2 cnt) m) (fast-expt-iter n m (* cnt 2) (* res res)))
        (else (* res (fast-expt-iter n (- m cnt) 1 n)))))

(define (fast-expt n m)
  (fast-expt-iter n m 1 n))