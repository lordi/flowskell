(shader (load-shader "blinn"))
; donuts
(define (tori n)
    (color yellow)
    (rotate (* (secs) 10) (vector 0 1 0))
    (rotate (* (secs) 10) (vector 1 0 0))
    (scale half)
    (color (hsv (% (+ (* (secs) 50)  (* n 50)) 360) 0.5 0.45))
    (draw-torus)
    (if (> n 1) (tori (- n 1))))

(every-frame
    (rotate -45 x-axis)
    (scale (towards1 0.5 (towards1 0.5 (sin (secs)))))
    (scale 0.75)
    (tori 10))
