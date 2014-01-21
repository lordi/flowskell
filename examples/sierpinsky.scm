;; slightly pulsating sierpinsky fractal
(define (sierpinsky n)
  (draw-triangle)
  (translate (vector 0 0 (+ (/ (sin (secs)) 10) 0)))
  (scale (+ (/ (sin (secs)) 60) 0.5))
  (push)
    (translate (vector -1 -1 0))
    (if (> n 0) (sierpinsky (- n 1)))
  (pop)
  (push)
    (translate (vector 0 1 0))
    (if (> n 0) (sierpinsky (- n 1)))
  (pop)
  (push)
    (translate (vector 1 -1 0))
    (if (> n 0) (sierpinsky (- n 1)))
  (pop))

(define (draw-triangle)
  (draw-line
    (list
        (vector -1 -1 0)
        (vector 0 1 0)
        (vector 1 -1 0)
        (vector -1 -1 0))))

(define (init)
  (scale 0.5)
  (rotate -45 x-axis)
  (translate (vector 0 0.2 0))
  (rotate (modulo (/ (msecs) 30) 360) z-axis))

(every-frame
  (init)
  (sierpinsky 4))
