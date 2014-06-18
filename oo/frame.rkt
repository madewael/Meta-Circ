#lang racket

(require racket/gui)

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Example"]))

(define vp (new vertical-panel% [parent frame]))

(define model-and-code (new horizontal-panel% [parent vp]))

(define input-panel (new horizontal-panel% [parent vp]))

(define input (new text-field% [parent input-panel]
     [label ""]))
    
(new button% [parent input-panel]
     [label "eval"]
     [callback (lambda (b e)(add-and-eval-code))])
 
 
; Make a canvas that handles events in the frame
(new canvas% [parent model-and-code]
     [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Don't Panic!" 0 0))])
(define code (new message% [parent (new vertical-panel% [parent model-and-code])]
                  	[auto-resize #t]
                  [label "(define (f x) (+ x 1))"]))
 


; Show the frame by calling its show method
(send frame show #t)

(define (add-and-eval-code)
  (let ((old-code (send code get-label))
        (new-expression (send input get-value)))
    (send input set-value "")
    (send code set-label 
          (pretty-format (string-append old-code "\n" new-expression)))))
  
  