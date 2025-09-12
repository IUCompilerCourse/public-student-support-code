#lang racket
(require "utilities.rkt")
(require "interp_Lwhile.rkt")
(provide interp_poly interp_poly-class)

(define interp_poly-class
  (class interp_Lwhile-class
    (super-new)

    (define/override ((interp_exp env) e)
      (define recur (interp_exp env))
      (verbose "poly/interp_exp" e)
      (match e
        [(Inst e ty ts) (recur e)]
        [else ((super interp_exp env) e)]))
    
    (define/override (interp-def d)
      (match d
        [(Poly ts (Def f (list `[,xs : ,ps] ...) rt _ body))
         (cons f (box (Function xs body '())))]
        [else (super interp-def d)]))
    
    ))

(define (interp_poly p)
  (send (new interp_poly-class) interp-program p))
