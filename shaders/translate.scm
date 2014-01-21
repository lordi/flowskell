;; Demonstrates the use of translate shader

;; Load and activate shader
(shader (load-shader "translate"))

;; Load and activate texture
(texture (load-texture "examples/img/smiley.png"))

(every-frame
  (rotate -45 x-axis)
  (color (hsv (/ (msecs) 20)))
  (set-uniform "x" 0.33)
  (set-uniform "y" 0.5)
  (draw-plane))
