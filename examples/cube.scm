; simple turning cube on a plane
(define every-frame
  (lambda ()
    (rotate -45.0 x-axis)
    (draw-plane)
    (rotate (modulo (/ (msecs) 20) 360) z-axis)
    (scale 0.2)
    (translate up)
    (draw-cube)))
