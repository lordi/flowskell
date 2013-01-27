; random test
(define every-frame
  (lambda ()
    (display (rndf))
    (display " ")
    (display (crndf))
    (display "\n")
    (display (vrnd))
    (display "\n")
    (display (vmul 0.1 (vrnd)))
    (display "\n")))

