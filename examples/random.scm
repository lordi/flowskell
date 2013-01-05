; random test
(define (rndf) (* (random 10000) 0.0001))
(define (crndf) (* (- (rndf) 0.5) 2))
(define every-frame
  (lambda ()
    (display (rndf))
    (display " ")
    (display (crndf))
    (display "\n")))
