TODO
(define my-cube
  (cons
    (scale 128 128 128)
    (translate 256 0 0)
    (make-cube)))
(define every-frame
  (cons
    (rotate (* 90 256) 0 0 256)
    (rotate (* 45 256) 0 256 0)
    (rotate (* (mod (/ (msecs) 10) 360) 256) 256 0 0)
    (translate 256 0 0)
    (scale 128 128 128)
    (make-cube)
    (translate 256 0 0)
    (scale 128 128 128)
    (make-cube)
    (translate 256 0 0)
    (scale 128 128 128)
    (make-cube)
    (translate 256 0 0)
    (scale 128 128 128)
    (make-cube)))
