(define (pr1-12-rec r c)
  (if (or (= c 0) (= c r))
      1
      (+ (pr1-12-rec (- r 1) (- c 1)) (pr1-12-rec (- r 1) c))))