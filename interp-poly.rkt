#lang racket
(require "utilities.rkt")
(require "interp-Lwhile.rkt")
(provide interp-poly interp-poly-class)

(define interp-poly-class
  (class interp-Lwhile-class
    (super-new)

    (define/override ((interp-exp env) e)
      (define recur (interp-exp env))
      (verbose "poly/interp-exp" e)
      (match e
        [(Inst e ty ts) (recur e)]
        [else ((super interp-exp env) e)]))
    
    (define/override (interp-def d)
      (match d
        [(Poly ts (Def f (list `[,xs : ,ps] ...) rt _ body))
         (cons f (box (Function xs body '())))]
        [else (super interp-def d)]))
    
    ))

(define (interp-poly p)
  (send (new interp-poly-class) interp-program p))
