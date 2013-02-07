; double helix
(define (spheres n)
    (rotate -30 z-axis)
    (translate (vector 0 0 2))
    (push)
        (push)
            (translate (vector -3 0 0))
            (scale 0.5)
            (color red)
            (draw-sphere)
        (pop)
        (color white)
        (draw-line (list (vector -3 0 0) (vector 3 0 0)))
        (push)
            (translate (vector 3 0 0))
            (scale 0.5)
            (draw-sphere)
        (pop)
    (pop)
    (if (> n 0) (spheres (- n 1))))
(define (every-frame)
    (scale 0.03)
    (rotate 55 x-axis)
    (rotate (* (secs) 10) x-axis)
    (rotate (* (sin (secs)) 360) z-axis)
    (translate (vector 0 0 (* -14 2)))
    (spheres 20))
