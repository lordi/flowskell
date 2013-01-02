(define every-frame
  (lambda ()
    (scale (/ (modulo (secs) 60.0) 60) (/ (modulo (secs) 60.0) 60) (/ (modulo (secs) 60.0) 60))
    (make-cube)))
