(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f
      ))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (if (= (smallest-divisor n) n)
      #t
      #f
      ))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (square n)
  (* n n))

(define (search-for-primes n m)
  (search-for-primes-rec n m))

(define (search-for-primes-rec num cnt)
  (if (not (= cnt 0))
      (if (timed-prime-test num)
          (search-for-primes-rec (+ num 1) (- cnt 1))
          (search-for-primes-rec (+ num 1) cnt)
      )))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))