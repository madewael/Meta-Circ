#lang racket

(provide load-lists

(require (only-in "interpreter-objects.rkt" cons-cel? cons-cel primitive-procedure)
         (prefix-in racket: racket))

(define *the-empty-list* '())

(define (load-lists env)
  (define def! (curryr define-variable! env))
  
  (for-each (curryr define-variable! env)
            '(cons car cdr)
            (map primitive-procedure
                 (list cons-cel cons-cel-car cons-cel-cdr))))
  