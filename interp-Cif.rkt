#lang racket
(require "utilities.rkt")
(require "interp-Rif.rkt")
(require "interp-Cvar.rkt")
(provide interp-Cif interp-Cif-mixin)

(define (interp-Cif-mixin super-class)
  (class super-class
    (super-new)
    (inherit interp-exp)

    (define/override (interp-stmt env)
      (lambda (s)
        (match s
          [(Assign (Var x) e)
           (cons (cons x ((interp-exp env) e)) env)]
          [else ((super interp-stmt env) s)]
          )))

    (define/override (interp-tail env CFG)
      (lambda (t)
        (match t
          [(Return e)
           ((interp-exp env) e)]
          [(Goto l)
           ((interp-tail env CFG) (dict-ref CFG l))]
          [(IfStmt (Prim op arg*) (Goto thn-label) (Goto els-label))
           (if ((interp-exp env) (Prim op arg*))
               ((interp-tail env CFG) (dict-ref CFG thn-label))
               ((interp-tail env CFG) (dict-ref CFG els-label)))]
          [(Seq s t2)
           (define new-env ((interp-stmt env) s))
           ((interp-tail new-env CFG) t2)]
          [else ((super interp-tail env CFG) t)]
          )))
    
    (define/override (interp-program p)
      (match p
        [(CProgram info G)
         ((interp-tail '() G) (dict-ref G 'start))]
        ))
    ))

(define (interp-Cif p)
  (define Cif-class (interp-Cif-mixin (interp-Cvar-mixin interp-Rif-class)))
  (send (new Cif-class) interp-program p))

  
