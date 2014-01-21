; recursion using snd-level
; make sure that you have Jack running and that the inputs are connected
(define cubes
  (lambda (n)
    (rotate (* (snd-level) 500) (vector 1.0 0.2 (cos (* (/ (secs) (+ n 1)) 10))))
    (translate (vector 0.0 (* (cos (snd-level)) 1.0) 0.0))
    (color (hsv (* n (* (snd-level) 300))))
    (draw-cube)
    (if (> n 0) (cubes (- n 1)))
  )
)
(define init
  (lambda ()
    (scale 0.05)
    (translate (vector 0.0 -1.0 0.0))
    (rotate 90.0 z-axis)
    (rotate 45.0 y-axis)
    (rotate (modulo (/ (msecs) 30) 360) x-axis)
    (rotate (modulo (/ (msecs) 100) 360) z-axis)))
(every-frame
  (lambda ()
    (init)
    (push)
    (cubes (min 150 (* (snd-level) 1000)))
    (pop)
    (scale 1.0 -1.0 -1.0)
    (cubes (min 150 (* (snd-level) 1000)))))
