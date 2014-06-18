#lang racket

(require "eval-scheme.rkt")

(define (print x)(display x)(newline))

(define (read-eval-print-loop)
  (let ((read (read)))
    (if (eof-object?  read)
        'done
        (begin
          (print(eval read *the-gobal-env*))
          (read-eval-print-loop)))))


(parameterize ((current-input-port (open-input-file "test-code/test-oo.rkt")))
  (read-eval-print-loop))
(read-eval-print-loop)