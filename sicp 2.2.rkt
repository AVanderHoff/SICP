(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let((top-left(beside up up))
             (bottom-right (below right right))
             (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let((smaller(up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (square-limit painter n)
  (let((quarter (corner-split painter n)))
    (let((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;(define right-split (split beside below))
;(define up-split (split below beside))

(define(split arg1 arg2)
  (lambda (painter n)(if (= n 0) painter (let((smaller((split arg1 arg2) painter (- n 1))))
                                  (arg1 painter (arg2 smaller smaller))))))
(define (right-split)(split beside below))
(define (up-split)(split below beside))

(define(frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect-v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge-frame2 frame))))))

(define (make-vector a b)(cons a b))
(define (xcor-vect a)(car a))
(define (ycor-vect a)(cdr a))
(define (add-vect x y)(make-vector (+ (xcor-vect x)(xcor-vect y)) (+ (ycor-vect x)(ycor-vect y))))
(define (sub-vect x y)(make-vector (- (xcor-vect x) (xcor-vect y)) (- (ycor-vect x)(ycor-vect y))))
(define (scale-vect s p)(make-vector (* s (xcor-vect p)) (* s (ycor-vect p))))
(define dean (make-vector 4 5))
(define kc (make-vector 6 7))

(define (make-frame origin edge1 edge2)
 (list origin edge1 edge2))
(define danny (make-frame 1 2 3))
(define (origin x)(car x))
(define (edge1 x)(cadr x))
(define (edge2 x)(caddr x))

(define (make-segment a b)(cons a b))
(define (start-segment x)(car x))
(define (end-segment x) (cdr x))

(define (segments->painter segment-list)
  (lambda(frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)(start-segment segment))
       ((frame-coord-map frame)(end-segment segment))))segment-list)))

(define x-help  (list (make-segment (make-vector 0.0 0.0) (make-vector .99 .99)) (make-segment (make-vector 0.0 .99)(make-vector .99 0.0))))

(define (trasform-painter painter origin corner1 corner2)
  (lambda(frame)
    (let((m(frame-coord-map frame)))
      (let((new-origin (m origin)))
        (painter 
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define(flop-vert painter)
  (transform-painter painter (make-vect 0.0 .99)
                             (make-vect .99 .99)
                            (make-vect 0.0 0.0)))



