#lang racket

(provide bounds
         bounds-x1 bounds-x2 bounds-y1 bounds-y2
         bounds-w bounds-h
         set-bounds-x1! set-bounds-x2! set-bounds-y1! set-bounds-y2!
         set-bounds-w! set-bounds-h!)

;     x       x+w
;   y +-------+
;     |       |
; y+h +-------+
;

(struct bounds ((x #:mutable) (y #:mutable) (w #:mutable) (h #:mutable)))

(define bounds-x1 bounds-x)
(define (bounds-x2 b)(+ (bounds-x1 b)(bounds-w b)))
(define bounds-y1 bounds-y)
(define (bounds-y2 b)(+ (bounds-y1 b)(bounds-h b)))

(define set-bounds-x1! set-bounds-x!)
(define (set-bounds-x2 b x2)
  (set-bounds-w! (- x2 (bounds-x1 b))))
(define set-bounds-y1! set-bounds-y!)
(define (set-bounds-y2 b y2)
  (set-bounds-h! (- y2 (bounds-y1 b))))

(define (bounds-shift-vertical! b n)
  (set-bounds-y! b (+ (bounds-y b) n)))

(define (bounds-shift-horizontal! b n)
  (set-bounds-x! b (+ (bounds-x b) n)))

(define (bounds-stretch-vertical! b n)
  (set-bounds-h! b (+ (bounds-h b) n)))

(define (bounds-stretch-horizontal! b n)
  (set-bounds-w! b (+ (bounds-w b) n)))





