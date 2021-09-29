#lang racket
(require racket/fixnum)
(require racket/dict)
(require "utilities.rkt")
(require "interp-Rint.rkt")
(provide interp-Rvar interp-Rvar-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Rvar-class
  (class interp-Rint-class
    (super-new)
    
    (define/override ((interp-exp env) e)
      (match e
        [(Var x) (dict-ref env x)]
        [(Let x e body)
         (define new-env (dict-set env x ((interp-exp env) e)))
         ((interp-exp new-env) body)]
        [else ((super interp-exp env) e)]
        ))

    ))

(define (interp-Rvar p)
  (send (new interp-Rvar-class) interp-program p))

