; simple turning cube on a grid
(define every-frame
  (lambda ()
    (scale 0.5)
    (rotate -45.0 x-axis)
    (rotate (/ (msecs) 100) z-axis)
    (draw-grid)
    (rotate (/ (msecs) 30) z-axis)
    (scale 0.5)
    (color gray)
    (draw-cube)))
