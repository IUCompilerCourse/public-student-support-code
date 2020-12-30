#lang racket
(require racket/fixnum)
(require racket/dict)
(require "utilities.rkt")
(provide interp-Rvar interp-Rvar-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Rvar-class
  (class object%
    (super-new)
    
    (define/public ((interp-exp env) e)
      (match e
        [(Int n) n]
        [(Prim 'read '())
         (define r (read))
         (cond [(fixnum? r) r]
               [else (error 'interp-exp "expected an integer" r)])]
        [(Prim '- (list e))
         (define v ((interp-exp env) e))
         (fx- 0 v)]
        [(Prim '+ (list e1 e2))
         (define v1 ((interp-exp env) e1))
         (define v2 ((interp-exp env) e2))
         (fx+ v1 v2)]
        [(Var x) (dict-ref env x)]
        [(Let x e body)
         (define new-env (dict-set env x ((interp-exp env) e)))
         ((interp-exp new-env) body)]
        ))

    (define/public (interp-program p)
      (match p
        [(Program '() e) ((interp-exp '()) e)]
        ))
    ))

(define (interp-Rvar p)
  (send (new interp-Rvar-class) interp-program p))

