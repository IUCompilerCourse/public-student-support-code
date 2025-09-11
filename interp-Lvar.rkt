#lang racket
(require racket/fixnum)
(require racket/dict)
(require "utilities.rkt")
(require "interp-Lint.rkt")
(provide interp_Lvar interp-Lvar-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Lvar-class
  (class interp-Lint-class
    (super-new)
    
    (define/override ((interp_exp env) e)
      (match e
        [(Var x) (dict-ref env x)]
        [(Let x e body)
         (define new-env (dict-set env x ((interp_exp env) e)))
         ((interp_exp new-env) body)]
        [else ((super interp_exp env) e)]
        ))

    ))

(define (interp_Lvar p)
  (send (new interp-Lvar-class) interp-program p))

