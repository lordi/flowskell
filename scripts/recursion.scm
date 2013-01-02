; tail recursion test
(define cubes
  (lambda (n)
    (rotate (* (sin (secs)) 20) 1.0 0.5 (cos (* (/ (secs) (+ n 1)) 10)))
    (translate 0.0 (* (sin (secs)) 1.0) 0.0)
    (make-cube)
    (if (> n 0) (cubes (- n 1)))
  )
)
(define init
  (lambda ()
    (scale 0.05 0.05 0.05)
    (translate 0.0 -1.0 0.0)
    (rotate 90.0 0.0 0.0 1.0)
    (rotate 45.0 0.0 1.0 0.0)
    (rotate (modulo (/ (msecs) 10) 360) 1.0 0.0 0.0)))
(define every-frame
  (lambda ()
    (init)
    (cubes 50)))
