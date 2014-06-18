#lang racket

(provide eval *the-gobal-env*)

(require "env.rkt" "lang.rkt" "interpreter-objects.rkt" "adaptive-eval.rkt"
         (prefix-in racket: racket))

(define *the-gobal-env* (extend-env
                         *the-empty-env*
                         '(< > + - * / = cons car cdr null?)
                         (map primitive-procedure (list < > + - * / = cons car cdr null?))))

(define (eval-number   number   env) number)

(define (eval-string   string   env) string)

(define (eval-variable variable env) (look-up-variable variable env))

(define (eval-if if-exp env)
  (let ((condition   (if-exp->condition   if-exp))
        (consequent  (if-exp->consequent  if-exp))
        (alternative (if-exp->alternative if-exp)))
    (if (eval condition   env)
        (eval consequent  env)
        (eval alternative env))))

(define (eval-cond cond-exp env)
  (eval (cond-exp->if-exp cond-exp) env))

(define (eval-var-definition definition-exp env)
  (let ((variable  (definition-exp->variable definition-exp))
        (value-exp (definition-exp->value-exp definition-exp)))
    (define-variable! variable (eval value-exp env) env)))

(define (eval-proc-definition definition-exp env)
  (eval (proc-definition->var-definition definition-exp) env))


(define (eval-lambda lambda-exp env)
  (procedure (lambda-exp->params lambda-exp)
             (exps->exp-sequence (lambda-exp->body lambda-exp))
             env))

(define (eval-let let-exp env)
  (let ((proc (procedure-from-let
               (let-exp->vars let-exp)
               (exps->exp-sequence (let-exp->body let-exp))
               env))
        (args (map (curryr eval env) (let-exp->value-exps let-exp))))
    (apply proc args)))


(define (eval-let* let*-exp env)
  (eval (let*-exp->let-exp let*-exp) env))

(define (eval-begin begin-exp env)
  (foldl (λ (exp res)(eval exp env))
         #f
         (begin->exps begin-exp)))

(define (eval-application application-exp env)
  (let ((procedure-exp (application-exp->procedure-exp application-exp))
        (arg-exps      (application-exp->arg-exps      application-exp)))
    (let ((proc (eval procedure-exp env))
          (args (map (curryr eval env) arg-exps)))
      (if (primitive-procedure? proc)
          (racket:apply (primitive-procedure-proc proc) args)
          (apply        proc args)))))

(define (eval-assignment assignment-exp env)
  (let ((var (assignment-exp->variable assignment-exp))
        (val (eval (assignment-exp->value-exp assignment-exp) env)))
    (set-variable! var val env)))

(define (eval-quote quote-exp env)
  (quote->datum quote-exp))

(define (apply proc args)
  (let ((env    (procedure-env    proc))
        (body   (procedure-body   proc))
        (params (procedure-params proc)))
    (let ((extended-env (extend-env env params args)))
      (eval body extended-env))))

(add-dedicated-eval application       application?      eval-application)
(add-dedicated-eval number            number?           eval-number)
(add-dedicated-eval string            string?           eval-string)
(add-dedicated-eval variable          symbol?           eval-variable)
(add-dedicated-eval if                if?               eval-if)
(add-dedicated-eval cond              cond?             eval-cond)
(add-dedicated-eval lambda            lambda?           eval-lambda)
(add-dedicated-eval let               let?              eval-let)
(add-dedicated-eval let*              let*?             eval-let*)
(add-dedicated-eval begin             begin?            eval-begin)
(add-dedicated-eval var-definition    var-definition?   eval-var-definition)
(add-dedicated-eval proc-definition   proc-definition?  eval-proc-definition)
(add-dedicated-eval assignment        assignment?       eval-assignment)
(add-dedicated-eval quote             quote?            eval-quote)




(define GE *the-gobal-env*)
