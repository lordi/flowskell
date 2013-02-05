; stupid helpers
; all of these may vanish / change soon
(define x-axis (vector 1 0 0))
(define y-axis (vector 0 1 0))
(define z-axis (vector 0 0 1))
(define up z-axis)
(define origin (vector 0 0 0))
(define % modulo)
(define half 0.5)
(define quarter 0.25)
(define towards1
  (lambda (scale n)
    (+ scale (* n scale))))

; vector helprs
; TODO: check for case vlen == 0
(define (vnorm vec)
  (vmul (/ 1 (vlen vec)) vec))

; random helpers
(define (rndf) (* (random 10000) 0.0001))
(define (crndf) (* (- (rndf) 0.5) 2))
(define (vrnd) (vector (crndf) (crndf) (crndf)))
(define (vrndn) (vnorm (vrnd)))

; colors
(define white   (vector 1 1 1))
(define gray    (vector 0.5 0.5 0.5))
(define black   (vector 0 0 0))
(define red     (vector 1 0 0))
(define green   (vector 0 1 0))
(define blue    (vector 0 0 1))
(define yellow  (vadd red green))
(define magenta (vadd red blue))
(define cyan    (vadd green blue))

; internal stuff
(define *source* "")        ; source file of the current script
(define *has-error* #f)     ; wether an error has occurred
(define every-frame-entry-point
    (lambda ()
      (if (not *has-error*)
        (every-frame))))
