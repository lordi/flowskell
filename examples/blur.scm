; fun with the blur filter
(blur 0.9)
(every-frame
  (rotate -45 x-axis)
  (rotate (modulo (/ (msecs) 20) 360) z-axis)
  (scale (+ 0.2 (* 0.05 (cos (secs)))))
  (color (hsv (* 20 (secs))))
  (draw-cube)
  (color (hsv (+ 145 (* 20 (secs)))))
  (translate up)
  (scale (* 0.9 (sin (secs))) 0.1 1.0)
  (draw-cube))
