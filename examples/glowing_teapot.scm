; glowing teapot
(define every-frame
  (lambda ()
    (scale 0.25 0.25 0.25)
    (rotate -45.0 0.5 0.0 0.0)
    (rotate (modulo (/ (msecs) 10) 360) (cos (/ (secs) 10)) 0.5 0.0)
    (color (sin (/ (secs) 1.2)) 0.5 0.5)
    (make-teapot)))
