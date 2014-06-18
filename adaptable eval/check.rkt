#lang racket

(provide check)

(require "env.rkt" "lang.rkt" "interpreter-objects.rkt"
         "type.rkt"
         (prefix-in racket: racket))

(define primitive-types
  (list (list '+ (make-proc-type (list int-type int-type) int-type))
        (list '- (make-proc-type (list int-type int-type) int-type))
        (list '* (make-proc-type (list int-type int-type) int-type))
        (list '/ (make-proc-type (list int-type int-type) int-type))
        (list '= (make-proc-type (list int-type int-type) boolean-type))))

(define *the-gobal-env* (extend-env
                         *the-empty-env*
                         (map car primitive-types)
                         (map cadr primitive-types)))

(define (check! expr type expected msg)
  (when (not (equal? type expected))
    (error (format "Type check of \"~a\" failed.\n~a,\nfound: ~a,\nexpected:~a"
             expr
             msg
             (type->string type)
             (type->string expected)))))

(define (check-number   number   env) int-type)

(define (check-string   string   env) string-type)

(define (check-variable variable env) (look-up-variable variable env))

(define (check-if if-exp env)
  (let ((condition   (if-exp->condition   if-exp))
        (consequent  (if-exp->consequent  if-exp))
        (alternative (if-exp->alternative if-exp)))
    (let ((condition-type (check condition env))
          (consequent-type (check consequent env))
          (alternative-type (check alternative env)))
      (check! if-exp condition-type boolean-type "")
      (check! if-exp consequent-type alternative-type "")
      consequent-type)))
    

(define (check-cond cond-exp env)
  (check (cond-exp->if-exp cond-exp) env))

(define (check-var-definition definition-exp env)
  (let ((variable  (definition-exp->variable definition-exp))
        (value-exp (definition-exp->value-exp definition-exp)))
    (define-variable! variable any-type env)
    (define-variable! variable (check value-exp env) env)))

(define (check-proc-definition definition-exp env)
  (check (proc-definition->var-definition definition-exp) env))

(define (check-lambda lambda-exp env)
  (let* ((params (lambda-exp->params lambda-exp))
         (body-as-sequence (exps->exp-sequence (lambda-exp->body lambda-exp)))
         (extended-env (extend-env env
                                   params
                                   (map (const any-type)params)))
         (return-type (check body-as-sequence extended-env)))
    (make-proc-type
     (map (λ (param) (look-up-variable param extended-env)) params)
     return-type)))

(define (check-let let-exp env)
  (let ((proc (procedure-from-let
               (let-exp->vars let-exp)
               (exps->exp-sequence (let-exp->body let-exp))
               env))
        (args (map (curryr check env) (let-exp->value-exps let-exp))))
    'todo))


(define (check-let* let*-exp env)
  (check (let*-exp->let-exp let*-exp) env))

(define (check-begin begin-exp env)
  (foldl (λ (exp res)(check exp env))
         #f
         (begin->exps begin-exp)))

(define (check-application application-exp env)
  (let ((procedure-exp (application-exp->procedure-exp application-exp))
        (arg-exps      (application-exp->arg-exps      application-exp)))
    (let ((proc-type (check procedure-exp env))
          (arg-types (map (curryr check env) arg-exps)))
      (for-each (λ (type expected)
                  (check! application-exp
                          type expected "arg mismatch"))   
                arg-types
                (proc-type->args-types proc-type))
      (proc-type->return-type proc-type))))


(struct exp-type (name test? checker))
(define exp-types
  (list ; Atomic
   (exp-type 'number           number?           check-number)
   (exp-type 'string           string?           check-string)
   (exp-type 'variable         symbol?           check-variable)
   ; Special Forms (pair-based)
   (exp-type 'if               if?               check-if)
   (exp-type 'cond             cond?             check-cond)
   (exp-type 'lambda           lambda?           check-lambda)
   (exp-type 'let              let?              check-let)
   (exp-type 'let*             let*?             check-let*)
   (exp-type 'begin            begin?            check-begin)
   (exp-type 'var-definition   var-definition?   check-var-definition)
   (exp-type 'proc-definition  proc-definition?  check-proc-definition)
   ; Application
   (exp-type 'application      application?      check-application)))

(define (exp->exp-type exp)
  (ormap (λ (exp-type)(and ((exp-type-test? exp-type) exp) exp-type))
         exp-types))

(define (check exp env)
  (let ((exp-type (exp->exp-type exp)))
    (if exp-type
        ((exp-type-checker exp-type) exp env)
        (error "Unknown expression type -- EVAL" exp))))

(define GE *the-gobal-env*)
;(check '(let ((a 1)(b 2))(+ a b)) GE)
(check '+ GE)
(check '(define (f x) 1 (+ x 1)) GE)
(check '(f 4) GE)
