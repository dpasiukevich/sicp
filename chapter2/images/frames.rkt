#lang sicp
(#%require sicp-pict)

(define (make-vect x y) (cons x y))
(define xcor-vect car)
(define ycor-vect cdr)

(define (modify v op dx dy)
  (make-vect (op (xcor-vect v) dx)
             (op (ycor-vect v) dy)))

(define (add-vect v1 v2) (modify v1 + (xcor-vect v2) (ycor-vect v2)))
(define (sub-vect v1 v2) (modify v1 - (xcor-vect v2) (ycor-vect v2)))
(define (scale-vect v s) (modify v * s s))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (draw-line start-vector end-vector) #t) ; just dummy method

(define (segments->painter segments)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segments)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))



