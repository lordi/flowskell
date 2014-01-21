;; Demonstrates the use of edge detection
(shader (load-shader "edge"))
(texture (load-texture "examples/img/smiley.png"))

(every-frame
  (rotate -45 x-axis)
  (draw-plane))
