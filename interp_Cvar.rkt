#lang racket
(require racket/fixnum)
(require racket/dict)
(require "utilities.rkt")
(require "interp_Lvar.rkt")
(provide interp_Cvar interp_Cvar-mixin)

(define (interp_Cvar-mixin super-class)
  (class super-class
    (super-new)
    (inherit interp_exp)

    (define/public (interp-stmt env)
      (lambda (s)
        (match s
          [(Assign (Var x) e)
           (dict-set env x ((interp_exp env) e))]
          [else
           (error 'interp-stmt "unmatched ~a" s)]
          )))

    (define/public (interp-tail env)
      (lambda (t)
        (match t
          [(Return e)
           ((interp_exp env) e)]
          [(Seq s t2)
           (define new-env ((interp-stmt env) s))
           ((interp-tail new-env) t2)]
          )))

    (define/override (interp-program p)
      (match p
        [(CProgram _ `((start . ,t)))
         ((interp-tail '()) t)]
        ))
    ))
    
(define (interp_Cvar p)
  (send (new (interp_Cvar-mixin interp_Lvar-class)) interp-program p))
