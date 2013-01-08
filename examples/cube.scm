; simple turning cube
(define every-frame
  (lambda ()
    (scale 0.25)
    (rotate -45.0 x-axis)
    (rotate (modulo (/ (msecs) 20) 360) z-axis)
    (make-cube)))
