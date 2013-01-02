; simple turning cube
(define every-frame
  (lambda ()
    (scale 0.25 0.25 0.25)
    (rotate -45.0 0.5 0.0 0.0)
    (rotate (modulo (/ (msecs) 10) 360) 0.0 0.5 0.0)
    (make-cube)))
