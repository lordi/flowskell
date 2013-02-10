; demonstrate vector math, vadd, vmul, vrnd
(define (every-frame)
    (scale 0.5)
    (rotate -45 x-axis)
    (rotate (/ (msecs) 30) z-axis)
    (draw-grid)
    (color (vadd (vmul 0.5 blue) (vmul (abs (cos (secs))) red)))
    (for-each
      (lambda (n)
        (draw-line
          (list origin (vmul (sin (secs)) (vrndn)) (vmul (sin (secs)) (vrndn)) origin)
          ))
      '(0 1 2 3 4 5 6 7 8 9)))
