(define (pr1-28 a b times)
  (define (cube x) (* x x x))
  (simpsons-rule cube a b times (/ (- b a) times)))

(define (simpsons-rule f a b n h)
  (define (simpsons-rule-iter cnt sum)
    (cond ((or (= cnt 0) (= cnt n)) (simpsons-rule-iter (+ cnt 1) (+ sum (f (+ a (* cnt h))))))
          ((= cnt (+ n 1)) (exact->inexact sum))
          ((even? cnt) (simpsons-rule-iter (+ cnt 1) (+ sum (* 2 (f (+ a (* cnt h)))))))
          (else (simpsons-rule-iter (+ cnt 1) (+ sum (* 4 (f (+ a (* cnt h)))))))))
  (/ (* h (simpsons-rule-iter 0 0)) 3))
              
  