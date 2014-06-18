#lang racket

(provide make-proc-type type->string
         any-type
         proc-type->args-types
         proc-type->return-type
         int-type boolean-type symbol-type string-type)

(define (type? t)
  (and (pair? t)
       (eq? (car t) 'type)))

(define base-type cadr)

(define (proc-type? t)
  (and (type? t)
       (eq? (base-type t) 'proc)))

(define (type-match? t1 t2)
  (or (equal? t1 any-type)
      (equal? t2 any-type)
      (equal? t1 t2)
      (and (proc-type? t1)
           (proc-type? t2)
           (type-match? (proc-type->return-type t1)
                        (proc-type->return-type t2))
           (or (equal? (proc-type->args-types t1))
               (equal? (proc-type->args-types t2))
               (andmap type-match?
                       (proc-type->args-types t1)
                       (proc-type->args-types t2))))))



(define proc-type->args-types caddr)
(define proc-type->return-type cadddr)

(define any-type '(type *))
(define int-type '(type int))
(define boolean-type '(type boolean))
(define symbol-type '(type symbol))
(define string-type '(type string))

(define (make-proc-type arg-types return-type)
  `(type proc ,arg-types ,return-type))

(define (type->string t)
  (if (proc-type? t)
      (format "~a -> ~a"
              (map type->string (proc-type->args-types t))
              (type->string (proc-type->return-type t)))
      (symbol->string (base-type t))))





