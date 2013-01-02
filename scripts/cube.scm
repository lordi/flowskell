(define every-frame
  (cons
    (scale 64 64 64)
    (rotate (* 90 256) 128 128 128)
    (rotate (* (mod (/ (msecs) 10) 360) 256) 128 0 0)
    (make-cube)))
