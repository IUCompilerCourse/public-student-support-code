#lang racket
(require racket/fixnum)
(require racket/dict)
(require "utilities.rkt")
(require "interp-R1.rkt")
(provide interp-C0)

(define (interp-C0-stmt env)
  (lambda (s)
    (match s
      [(Assign (Var x) e)
       (dict-set env x ((interp-exp env) e))]
      )))

(define (interp-C0-tail env)
  (lambda (t)
    (match t
      [(Return e)
       ((interp-exp env) e)]
      [(Seq s t2)
       (define new-env ((interp-C0-stmt env) s))
       ((interp-C0-tail new-env) t2)]
      )))
  
(define (interp-C0 p)
  (match p
    [(Program _ (CFG `((start . ,t))))
     ((interp-C0-tail '()) t)]
    ))
