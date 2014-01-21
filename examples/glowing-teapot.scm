; glowing teapot
(every-frame
  (lambda ()
    (scale 0.25)
    (rotate -45 x-axis)
    (rotate (/ (msecs) 10) (vector (cos (/ (secs) 10)) 0.5 0))
    (color (vector (sin (/ (secs) 1.2)) 0.5 0.5))
    (draw-teapot)))
