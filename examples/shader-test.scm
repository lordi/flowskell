(define *red-tint* (load-shader "red-tint"))
(define *fade* (load-shader "fade"))
(define *t* (load-shader "texture"))
(define *smiley* (load-texture "examples/img/smiley.png"))
(define (setup-view)
        (scale 0.25)
        (rotate -45.0 x-axis)
        (rotate (modulo (/ (msecs) 20) 360) z-axis))
(define (unify n) (+ 0.5 (* 0.5 n)))
(define (every-frame)
    (texture 0)
    (color white)
    (shader)
    (push)
        (scale (+ 1 (* 0.5 (sin (secs)))))
        (draw-plane)
    (pop)

    (texture 0)
    ;; First shader: red-tint
    (push)
        (shader *red-tint*)
        (set-uniform "alpha" (unify (sin (secs))))
        (texture)
        (translate (vector 0.5 0.5 0.5))
        (setup-view)
        (draw-cube)
    (pop)

    (push)
        (shader *red-tint*)
        (set-uniform "alpha" (unify (sin (secs))))
        (texture)
        (translate (vector -0.5 0.5 0.5))
        (setup-view)
        (draw-cube)
    (pop)

    (push)
        (shader *t*)
        (texture *smiley*)
        (set-uniform "alpha" (unify (sin (secs))))
        (translate (vector -0.5 -0.5 0.5))
        (setup-view)
        (draw-cube)
    (pop)


    )
