#lang racket
(require "utilities.rkt")
(require "interp-Lif.rkt")
(require "interp-Cvar.rkt")
(provide interp-Cif interp-Cif-mixin)

(define (interp-Cif-mixin super-class)
  (class super-class
    (super-new)
    (inherit interp_exp)

    (define/override (interp-stmt env)
      (lambda (s)
        (match s
          [(Assign (Var x) e)
           (cons (cons x ((interp_exp env) e)) env)]
          [else ((super interp-stmt env) s)]
          )))

    (define/override (interp-tail env blocks)
      (lambda (t)
        (match t
          ;; Cvar cases, repeated logic but with blocks added
          [(Return e)
           ((interp_exp env) e)]
          [(Seq s t2)
           (define new-env ((interp-stmt env) s))
           ((interp-tail new-env blocks) t2)]
          ;; Cif cases
          [(Goto l)
           ((interp-tail env blocks) (dict-ref blocks l))]
          [(IfStmt (Prim op arg*) (Goto thn-label) (Goto els-label))
           (if ((interp_exp env) (Prim op arg*))
               ((interp-tail env blocks) (dict-ref blocks thn-label))
               ((interp-tail env blocks) (dict-ref blocks els-label)))]
          )))
    
    (define/override (interp-program p)
      (match p
        [(CProgram info blocks)
         ((interp-tail '() blocks) (dict-ref blocks 'start))]
        ))
    ))

(define (interp-Cif p)
  (define Cif-class (interp-Cif-mixin (interp-Cvar-mixin interp-Lif-class)))
  (send (new Cif-class) interp-program p))

  
