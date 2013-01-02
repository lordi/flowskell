(define every-frame
  (cons
    (scale 0.25 0.25 0.25)
    (rotate 90.0 0.5 0.5 0.5)
    (rotate (modulo (/ (msecs) 10) 360) 0.5 0.0 0.0)
    (make-cube)))
