#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp-Lif.rkt")
(provide interp-Lwhile interp-Lwhile-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Lwhile-class
  (class interp-Lif-class
    (super-new)

    (define/override ((interp-exp env) e)
      (verbose "Lwhile/interp-exp" e)
      (define recur (interp-exp env))
      (define result
      (match e
        [(Let x e body)
         (define new-env (dict-set env x (box (recur e))))
         ((interp-exp new-env) body)]
        [(Var x) (unbox (dict-ref env x))]
        [(GetBang x) (unbox (dict-ref env x))]
        [(SetBang x rhs)
         (set-box! (dict-ref env x) (recur rhs))]
        [(WhileLoop cnd body)
         (define (loop)
           (cond [(recur cnd)  (recur body) (loop)]
                 [else         (void)]))
         (loop)]
        [(Begin es body)
         (for ([e es]) (recur e))
         (recur body)]
        [(Void)  (void)]
        [else ((super interp-exp env) e)]))
      (verbose "Lwhile/interp-exp" e result)
      result)
    ))

(define (interp-Lwhile p)
  (send (new interp-Lwhile-class) interp-program p))

