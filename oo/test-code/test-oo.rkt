(define-class account (balance min)
  (define (withdraw amount)
    (if (< (- balance amount) min)
        'fail
        (set! balance (- balance amount))))
  (define (deposit amount)
    (set! balance (+ balance amount))))

(define a (account 0 -1000))

(a get-balance)
(a deposit 100)
(a get-balance)
(a withdraw 1000)
(a get-balance)
(a withdraw 250)