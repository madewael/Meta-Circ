#lang racket

(provide primitive-procedure? primitive-procedure primitive-procedure-proc
         procedure? procedure procedure-params procedure-body procedure-env
         procedure-from-let procedure-from-let?
         tagged-procedure tagged-procedure?
         cons-cel? cons-cel
         class? class class-fields class-method-definitions class-env
         object? object object-class object-env
         method? method)
         

(struct primitive-procedure (proc))

(struct procedure (params body env (tag #:auto #:mutable)) #:auto-value 'procedure)

(define (tagged-procedure params body env tag)
  (let ((proc (procedure params body env)))
    (set-procedure-tag! proc tag)
    proc))

(define (tagged-procedure? x tag)
  (and (procedure? x)
       (eq? (procedure-tag x) tag)))
  


(define (procedure-from-let params body env)
  (tagged-procedure params body env 'let))

(define (procedure-from-let? x)(tagged-procedure? x 'let))

(struct cons-cel ((car #:mutable)(cdr #:mutable)))


(struct class (fields method-definitions env))
(struct object (class env))

(define (method params body env)
  (tagged-procedure params body env 'method))
(define (method? x)(tagged-procedure? x 'method))