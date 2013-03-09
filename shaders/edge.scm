;;
;; Demonstrates the use of edge detection
;;

;; Load and activate shader
(shader (load-shader "edge"))
(define t (load-texture "examples/img/smiley.png"))

(define (every-frame)
  (rotate -45 x-axis)
  (color (hsv (/ (msecs) 20)))
  (texture t)
  (draw-plane))
