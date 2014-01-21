; moving tunnel test
(define (cubes n frac)
    (translate (vector 0 1 0))
    (color (hsv (* (- (+ n frac) (secs)) 25) 1 (/ n 60)))
    (push)
        (scale 0.5)
        (draw-cube)
    (pop)
    (push)
        (translate (vector 1 0 0.5))
        (rotate 45 (vector 0 1 0))
        (scale 0.5)
        (draw-cube)
    (pop)
    (push)
        (translate (vector -1 0 0.5))
        (rotate -45 (vector 0 1 0))
        (scale 0.5)
        (draw-cube)
    (pop)
    (if (> n 0) (cubes (- n 1) frac)))
(define (init)
    (scale 0.65)
    (translate (vector 0 -1 0))
    (rotate -80 x-axis))
(every-frame
    (init)
    (translate (vector 0 -1 0))
    (translate (vector 0 (- 0 (% (secs) 1)) 0))
    (cubes 30 (% (secs) 1)))
