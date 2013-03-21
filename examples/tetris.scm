; Needs a least husk 3.7.1

; Return a list containing numbers from 0 to n
(define (range n)
    (if (> n 0)
        (append (range (- n 1)) (list n))
        '(0)))

; Load block textures from 0 to 15
(define *blocks*
    (map (lambda (n)
        (load-texture (string-append "examples/img/blocks/block-" (number->string n) ".png"))
        )
    (range 6)))


(define *curx* 0)
(define *cury* 0)
(define *curitem* 0)
(define *width* 10)
(define *height* 12)
(define *last-emerge* (secs))
(define *field*
  (list
    (list 0 0 0 0 0 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 0 0 0)))
(define *items*
  (list
      (list
        (list 1 0)
        (list 1 0)
        (list 1 1))
      (list
        (list 6 6)
        (list 6 0)
        (list 6 0))
      (list
        (list 4 4)
        (list 4 4))
      (list
        (list 3 3 0)
        (list 0 3 3))
      (list
        (list 0 5 5)
        (list 5 5 0))
      (list
        (list 2)
        (list 2)
        (list 2)
        (list 2))))

;; Utility functions for a 2D field
(define (field-height f) (length f))
(define (field-width f) (length (car f)))
(define (field-map func f)
  (map (lambda (y line)
    (map
      (lambda (x item) (func x y item))
      (range (field-width f)) line))
    (range (field-height f)) f))

(define (draw-box b)
  (if (> b 0)
      (list (push)
      (scale 0.5)
      (color (hsv (* b 36)))
      (texture (list-ref *blocks* b))
      (draw-cube)
      (pop))))
(define (draw-line l)
  (push)
  (map (lambda (r) 
    (translate (vector 1 0 0))
    (draw-box r)) (reverse l))
  (pop))
(define (draw-field f)
  (push)
  (map (lambda (l)
    (translate (vector 0 1 0))
    (draw-line l)) f)
  (pop))
(define (draw-falling)
  (let ((x *curx*)
        (y *cury*))
      (push)
      (translate (vector x y 0))
      (draw-field (list-ref *items* *curitem*))
      (pop)))
;;      (push)
;;      (translate (vector x (floor y) 0))
;;      (draw-field (list-ref *items* *curitem*))
;;      (pop)))
;; http://stackoverflow.com/questions/1760406/scheme-how-do-i-modify-an-individual-element-in-a-list
(define (list-set l k obj)
  (cond
    ((or (< k 0) (null? l)) #f)
    ((= k 0) (set-car! l obj))
    (else (append (list (car l)) (list-set (cdr l) (- k 1) obj)))))
(define (field-set! x y type)
  (set! *field* (list-set *field* y (list-set (list-ref *field* y) x type))))
(define (manifest-item source x y)
  (field-map (lambda (x_ y_ type) 
        (if (> type 0) (field-set! (+ x x_) (+ y y_) type))) source))

(define (every-frame)
  (set! *cury* (- *height* (* 4 (- (secs) *last-emerge*))))
  (if (<= *cury* 0)
    (let ((item (list-ref *items* *curitem*)))
        (manifest-item item *curx* (- (min *height* (- *height* *cury*)) (field-height item)))
        (set! *last-emerge* (secs))
        (set! *curx* (random (- *width* 3)))
        (set! *curitem* (random (- (length *items*) 1)))
        (set! *cury* *height*)))
  (push)
    (color (vmul 0.2 white))
    (translate (vector 0 0 -0.1))
    (draw-plane)
  (pop)
  (scale 0.15)
  (translate (vector (/ *width* -2) (/ *height* -2) 0))
  (draw-field *field*)
  (draw-falling))


(display "Field dimensions: ")
(display (field-width *field*))
(display "x")
(display (field-height *field*))
(newline)
