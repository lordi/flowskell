; solar system (not to scale)
(define moon (lambda ()
    (color gray)
    (draw-sphere)))

(define earth (lambda ()
    (color blue)
    (draw-sphere)
    (rotate (% (/ (msecs) 10.0) 360.0) (vector 1.0 0.0 0.0))
    (translate (vector 0.0 2.0 0.0))
    (draw-grid)
    (scale 0.3)
    (moon)))

(define mars (lambda ()
    (color red)
    (draw-sphere)))

(define sun (lambda ()
    (color yellow)
    (draw-sphere)
    (push)
        (rotate (% (/ (msecs) 20.0) 360.0) (vector 0.0 1.0 0.0))
        (translate (vector 2.5 0.0 0.0))
        (draw-grid)
        (scale 0.4)
        (earth)
    (pop)
    (push)
        (rotate (% (/ (msecs) 20.0) 360.0) (vector 0.0 0.0 1.0))
        (translate (vector 1.4 0.0 0.0))
        (draw-grid)
        (scale 0.18)
        (mars)
    (pop)))

(define every-frame
  (lambda ()
    (rotate -45.0 x-axis)
    (draw-grid)
    (scale 0.25)
    (sun)))