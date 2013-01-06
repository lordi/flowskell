(define every-frame
  (cons
    (color 100 0 (mod (msecs) 255))
    (make-line (msecs))
    (scale (mod (/ (msecs) 20) 64) (mod (/ (msecs) 10) 64) 255)
    (make-cube)
    (rotate (* (msecs) 25) 0 0 255)
    (make-cube)
    (color (mod (msecs) 255) 20 100)
    (translate 0 (* (mod (/ (msecs) 100) 64) 20) 0)
    (make-cube)))
