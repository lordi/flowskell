(define start-time (secs))
(define fps 0)
(define (every-frame)
  (display "after ")
  (display (floor (- (secs) start-time)))
  (display " seconds: ")
  (display (/ fps (- (secs) start-time)))
  (display " fps")
  (set! fps (+ fps 1))
  (newline))
