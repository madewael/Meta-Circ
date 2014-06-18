#lang racket

(require "env.rkt" "interpreter-objects.rkt")

(provide class-definition? eval-class-definition)

(define (class-definition? exp)
  
         

(struct class (constructor))
(struct object (class field-values))


(define (method params body env)
  (tagged-procedure params body env 'method))

(define (method? x)
  (tagged-procedure? x 'method))

(define (class-definition class-name field-names method-definitions env)
  (define-variable! class-name (class 'constructor) env))
  