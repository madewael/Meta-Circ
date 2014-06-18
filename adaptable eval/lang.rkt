#lang racket

(provide if?
         if-exp->condition
         if-exp->consequent
         if-exp->alternative
         
         cond?
         cond-exp->if-exp
         
         var-definition?
         definition-exp->variable
         definition-exp->value-exp
         
         proc-definition?
         definition-exp->procedure-name
         definition-exp->arg-list
         definition-exp->body
         proc-definition->var-definition
         
         class-definition?
         definition-exp->class-name
         definition-exp->field-names
         definition-exp->method-definitions
         
         assignment?
         assignment-exp->variable
         assignment-exp->value-exp
         
         lambda?
         lambda-exp->params
         lambda-exp->body
         
         let?
         let-exp->var-val-pairs
         let-exp->vars
         let-exp->value-exps
         let-exp->body
         
         let*?
         let*-exp->var-val-pairs
         let*-exp->vars
         let*-exp->value-exps
         let*-exp->body
         let*-exp->let-exp
         
         begin?
         begin->exps
         exps->exp-sequence
         
         application?
         application-exp->procedure-exp
         application-exp->arg-exps
         
         quote?
         quote->datum)


(define (tagged-list? tag lst)
  (and (pair? lst)(eq? (car lst) tag)))


(define definition?  (curry tagged-list? 'define))


;(if condition consequent alternative)
(define if? (curry tagged-list? 'if))
(define if-exp->condition   cadr)
(define if-exp->consequent  caddr)
(define if-exp->alternative cadddr)

;(cond (test e1 e2 ...)(test2 e3 e4 ...)(else e5 e6 ...))
(define cond? (curry tagged-list? 'cond))
(define cond-exp->clauses cdr)
(define clause->test car)
(define clause->exps cdr)
(define (else-clause? clause)(eq? (car clause) 'else))

(define (cond-exp->if-exp cond-exp)
  (let loop ((clauses (cond-exp->clauses cond-exp)))
    (cond
      ((null? clauses) '())
      ((else-clause? (car clauses))
       (when (not (null? (cdr clauses)))
         (error "else clause must be last clause" cond-exp))
       (exps->exp-sequence (clause->exps (car clauses))))
      (else `(if ,(clause->test (car clauses))
                 (begin ,@(exps->exp-sequence (clause->exps (car clauses))))
                 ,(loop (cdr clauses)))))))



;(define variable value-exp)
(define (var-definition? exp)(and (definition? exp)
                                  (symbol? (cadr exp))))
(define definition-exp->variable cadr)
(define definition-exp->value-exp caddr)


;(define (function-name . arg-list) body ...)
(define (proc-definition? exp)(and (definition? exp)
                                   (pair? (cadr exp))))
(define definition-exp->procedure-name caadr)
(define definition-exp->arg-list       cdadr)
(define definition-exp->body           cddr)
(define (proc-definition->var-definition definition-exp)
  (let ((procedure-name (definition-exp->procedure-name definition-exp))
        (arg-list       (definition-exp->arg-list       definition-exp))
        (body           (definition-exp->body           definition-exp)))
    `(define ,procedure-name (lambda ,arg-list ,@body))))

;(define-class class-name field-names method-definitions)
(define class-definition? (curry tagged-list? 'define-class))
(define definition-exp->class-name         cadr)
(define definition-exp->field-names        caddr)
(define definition-exp->method-definitions cdddr)

;(set! var val)
(define assignment? (curry tagged-list? 'set!))
(define assignment-exp->variable cadr)
(define assignment-exp->value-exp caddr)

;(lambda params body)
(define lambda? (curry tagged-list? 'lambda))
(define lambda-exp->params cadr)
(define lambda-exp->body   cddr)

;(let ((v1 e1)(v2 e2)...) body...)
(define let? (curry tagged-list? 'let))
(define let-exp->var-val-pairs cadr)
(define var-val-pair->var      car)
(define var-val-pair->val      cadr)
(define let-exp->vars          (compose (curry map var-val-pair->var)  let-exp->var-val-pairs))
(define let-exp->value-exps    (compose (curry map var-val-pair->val)  let-exp->var-val-pairs))
(define let-exp->body          cddr)

;(let* ((v1 e1)(v2 e2)...) body...)
(define let*? (curry tagged-list? 'let*))
(define let*-exp->var-val-pairs let-exp->var-val-pairs)
(define let*-exp->vars          let-exp->vars)
(define let*-exp->value-exps    let-exp->value-exps)
(define let*-exp->body          let-exp->body)


;(begin exps)
(define begin? (curry tagged-list? 'begin))
(define begin->exps cdr)

(define (let*-exp->let-exp let*-exp)
  (let loop ((var-val-pairs (let*-exp->var-val-pairs let*-exp)))
    (cond
      ((null? var-val-pairs)
       `(let () ,@(let*-exp->body let*-exp)))
      ((null? (cdr var-val-pairs))
       `(let (,(car var-val-pairs)) ,@(let*-exp->body let*-exp)))
      (else 
       `(let (,(car var-val-pairs))
          ,(loop (cdr var-val-pairs)))))))

(define (exps->exp-sequence exps)
  (if (null? (cdr exps))
      (car exps)
      `(begin ,@exps)))


;(procedure-name . argument-expressions)
(define application? pair?)
(define application-exp->procedure-exp car)
(define application-exp->arg-exps      cdr)

;(quote datum)
(define quote?  (curry tagged-list? 'quote))
(define quote->datum cadr)
