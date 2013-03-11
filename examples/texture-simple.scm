(define t (load-texture "examples/img/haskell-logo.png"))
(define (every-frame)
  (scale 0.4)
  (translate (vmul 2 left))

  ;; normal texture
  (texture t)
  (draw-plane)

  ;; last frame
  (translate (vmul 2 right))
  (texture 1)
  (draw-plane)

  ;; no texture, paint red
  (translate (vmul 2 right))
  (texture 0)
  (color red)
  (draw-plane)
  )
