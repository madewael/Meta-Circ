#lang racket

(provide eval add-dedicated-eval)

(require "env.rkt" "lang.rkt" "interpreter-objects.rkt"
         (prefix-in racket: racket))

(struct exp-type (name test? evaluator))

(define exp-types '())
(define (add-exp-type exp-type)
  (set! exp-types (cons exp-type exp-types)))

(define-syntax add-dedicated-eval
  (syntax-rules ()
    ((add-dedicated-eval name test? evaluator)(add-exp-type (exp-type (quote name) test? evaluator)))))


(define (exp->exp-type exp)
  (ormap (λ (exp-type)(and ((exp-type-test? exp-type) exp) exp-type))
         exp-types))

(define (eval exp env)
  (let ((exp-type (exp->exp-type exp)))
    (if exp-type
        ((exp-type-evaluator exp-type) exp env)
        (error "Unknown expression type -- EVAL" exp))))
