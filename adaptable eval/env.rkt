#lang racket

(require "interpreter-objects.rkt")
(provide  *the-empty-env* look-up-variable define-variable! set-variable!
          frame-parent look-up-binding binding-value
          extend-env)

(struct binding (variable (value #:mutable)))
(struct frame   ((bindings #:mutable) parent (children #:auto #:mutable))
  #:auto-value '())
(define (add-frame-binding! frame variable value)
  (set-frame-bindings! frame (cons (binding variable value)
                                   (frame-bindings frame))))

(define *the-empty-env* (frame #f #f))
(define (empty-env? env) (eq? env *the-empty-env*))


(define (look-up-binding variable frame)
  (ormap (λ (binding)(and (eq? variable (binding-variable binding))
                          binding))
         (frame-bindings frame)))


(define (look-up-variable variable env)
  (if (empty-env? env)
      (error "Unbound variable" variable)
      (let ((binding (look-up-binding variable env)))
        (if binding
            (binding-value binding)
            (look-up-variable variable (frame-parent env))))))

(define (define-variable! variable value env)
  (when (empty-env? env) (error "Can not set variable in empty env!" variable value))
  (let ((binding (look-up-binding variable env)))
    (if binding
        (set-binding-value! binding value)
        (add-frame-binding! env variable value))))

(define (set-variable! variable value env)
  (if (empty-env? env)
      (error "Unbound variable" variable)
      (let ((binding (look-up-binding variable env)))
        (if binding
            (set-binding-value! binding value)
            (set-variable! variable value (frame-parent env))))))

(define (add-child-frame parent-env child-env)
  (set-frame-children! parent-env
                       (cons child-env (frame-children parent-env))))

(define (extend-env env vars vals)
  (let ((f (frame (map binding vars vals) env)))
    (add-child-frame env f)
    f))

(define (visit-frame frame
                     proc-combine
                     proc-binding
                     proc-child
                     proc-parent)
  (proc-combine (map proc-binding (frame-bindings frame))
                (map proc-child   (frame-children frame))
                (proc-parent (frame-parent frame))))

