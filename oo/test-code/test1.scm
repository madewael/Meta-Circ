(define (f x) (+ x 1))

(define (map g lst)
  (if (null? lst)
      '()
      (cons (g (car lst))
            (map g (cdr lst)))))

(let ((l (cons 1 (cons 2 (cons 3 '()))))
      (p f))
  (map p l))