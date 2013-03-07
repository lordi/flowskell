;;
;; Demonstrates the use of four different GLSL shaders
;;

;; Load shaders
(define *tint* (load-shader "tint"))
(define *texture* (load-shader "texture"))
(define *plasma* (load-shader "plasma"))
(define *translate* (load-shader "translate"))
(define *edge* (load-shader "edge"))

;; Load textures
(define *smiley* (load-texture "examples/img/smiley.png"))

;; Helper functions
(define (setup-view)
    (scale 0.25)
    (rotate -45.0 x-axis)
    (rotate (modulo (/ (msecs) 20) 360) z-axis))
(define (unify n) (+ 0.5 (* 0.5 n)))

;; Construct the scene
(define (every-frame)
    (texture)
    (color white)
    (push)
        (scale (+ 1.2 (* 0.25 (sin (secs)))))
        (draw-plane)
    (pop)
    (texture *smiley*)

    (push)
        (shader *tint*)
        (set-uniform "alpha" (max 0.2 (unify (cos (secs)))))
        (set-uniform "tintColor" (hsv (/ (msecs) 10.0)))
        (translate (vector 0.5 0.5 0.5))
        (setup-view)
        (draw-cube)
    (pop)

    (push)
        (shader *edge*)
        (texture *smiley*)
        (set-uniform "x" (% (secs) 1))
        (set-uniform "y" (sin (secs)))
        (translate (vector -0.5 0.5 0.5))
        (setup-view)
        (draw-cube)
    (pop)

    (push)
        (shader *translate*)
        (set-uniform "x" (% (secs) 1))
        (set-uniform "y" (sin (secs)))
        (translate (vector 0.5 -0.5 0.5))
        (setup-view)
        (draw-cube)
    (pop)

    (texture 0)
    (push)
        (shader *plasma*)
        (set-uniform "time" (* 10.0 (secs)))
        (translate (vector -0.5 -0.5 0.5))
        (setup-view)
        (draw-cube)
    (pop)
    )

(display "*********************************************") (newline)
(display "* GLSL Shader demonstration") (newline)
(display "*********************************************") (newline)
(display "* Top left: Edge detection") (newline)
(display "* Top right: Tint texture with color+alpha") (newline)
(display "* Bottom left: Plasma like frag coloring") (newline)
(display "* Bottom right: Translate texture") (newline)
(display "*********************************************") (newline)
