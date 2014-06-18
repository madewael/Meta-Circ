#lang racket

(require "interpreter-objects.rkt" racket/draw)

(struct drawable-procedure (x y proc))


(define (params+body-string proc)
    (format "Paramerers:\n~a\n\nBody:\n~a"
                 (procedure-params proc)
                 (procedure-body proc)))

(define (draw-procedure drawable-proc dc)
  (let ((x 0)(y 0)(w 100)(h 30)(proc (drawable-procedure-proc drawable-proc)))
    (send dc draw-rectangle x y w h)
    (send dc draw-text (params+body-string proc) x y)))

(struct draw-type (name test? drawer))
(define draw-types
  (list
   (draw-type 'procedure drawable-procedure? draw-procedure)))

(define (drawable->draw-type drawable)
  (ormap (λ (draw-type)(and ((draw-type-test? draw-type) drawable) draw-type))
         draw-types))

(define (draw drawable dc)
  (let ((draw-type (drawable->draw-type drawable)))
    (if draw-type
        ((draw-type-drawer draw-type) drawable dc)
        (error "Unkown draw-type" drawable))))


;;;;;
(define target (make-bitmap 500 500)) ; A 30x30 bitmap
(define dc (new bitmap-dc% [bitmap target]))
;(draw (drawable-procedure 0 0 (procedure '(a b) '(+ a b) '())) dc)
