; stupid helpers
; all of these may vanish / change soon
(define x-axis (vector 1.0 0.0 0.0))
(define y-axis (vector 0.0 1.0 0.0))
(define z-axis (vector 0.0 0.0 1.0))
(define origin (vector 0.0 0.0 0.0))
(define % modulo)
(define half 0.5)
(define quarter 0.25)
(define towards1
  (lambda (scale n)
    (+ scale (* n scale))))

(define white (vector 1.0 1.0 1.0))
(define gray (vector 0.5 0.5 0.5))
(define black (vector 0.0 0.0 0.0))
(define red (vector 1.0 0.0 0.0))
(define green (vector 0.0 1.0 0.0))
(define blue (vector 0.0 0.0 1.0))
(define yellow (vector 1.0 1.0 0.0))

