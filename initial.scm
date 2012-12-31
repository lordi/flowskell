(define every-frame (cons (color (mod (msecs) 255) 200 100) (make-cube) (color 100 0 (mod (msecs) 255)) (make-line (msecs))))
