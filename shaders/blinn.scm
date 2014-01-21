;; Demonstrates the use of the blinn shader
(shader (load-shader "blinn"))
(every-frame
  (rotate 145 y-axis)
  (translate (vector 0.5 0 0))
  (rotate 145 x-axis)
  (color (hsv 130))
  (scale 0.38)
  (draw-sphere)
  (translate left)
  (translate left)
  (translate left)
  (color red)
  (draw-sphere))
