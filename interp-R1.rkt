#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp-R1 interp-C0)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define (interp-exp env)
  (lambda (e)
    (match e
      [(Int n) n]
      [(Prim 'read '())
       (define r (read))
       (cond [(fixnum? r) r]
             [else (error 'interp-R1 "expected an integer" r)])]
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
      )))

(define (interp-R1 p)
  (match p
    [(Program info e) ((interp-exp '()) e)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
