;;
;; Demonstrates the use of edge detection
;;

;; Load and activate shader
(shader (load-shader "translate"))
(define t (load-texture "examples/img/smiley.png"))

(define (every-frame)
  (rotate -45 x-axis)
  (color (hsv (/ (msecs) 20)))
  (set-uniform "x" 0.33)
  (set-uniform "y" 0.5)
  (texture t)
  (draw-plane))
