;;
;; Demonstrates the use of smooth-edges shader
;;

;; Load and activate shader
(shader (load-shader "smooth-edges"))

;; Draw three planes
(define (every-frame)
  (rotate -45 x-axis)
  (color (hsv (/ (msecs) 20)))
  (draw-plane)
  (push)
      (translate (vmul 2 right))
      (color (hsv (+ 90 (/ (msecs) 20))))
      (draw-plane)
  (pop)
  (push)
      (translate (vmul 2 left))
      (color (hsv (- 90 (/ (msecs) 20))))
      (draw-plane)
  (pop))
