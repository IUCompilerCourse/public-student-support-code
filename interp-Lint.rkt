#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp_Lint interp-Lint-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter for Lint: integer arithmetic

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code. This code does not use
;;   the match 'app' feature because the book doesn't introduce
;;   that until a later.

(define (interp_exp e)
    (match e
      [(Int n) n]
      [(Prim 'read '())
       (define r (read))
       (cond [(fixnum? r) r]
             [else (error 'interp_exp "expected an integer" r)])]
      [(Prim '- (list e))
       (define v (interp_exp e))
       (fx- 0 v)]
      [(Prim '+ (list e1 e2))
       (define v1 (interp_exp e1))
       (define v2 (interp_exp e2))
       (fx+ v1 v2)]
      [(Prim '- (list e1 e2))
       (define v1 (interp_exp e1))
       (define v2 (interp_exp e2))
       (fx- v1 v2)]
      ))

(define (interp_Lint p)
  (match p
    [(Program '() e) (interp_exp e)]
    ))


;; This version of the interpreter for Lint is the base class
;; for interp-Rvar-class in interp-Rvar.rkt.

(define interp-Lint-class
  (class object%
    (super-new)
    
    (define/public ((interp_exp env) e)
      (match e
        [(Int n) n]
        [(Prim 'read '())
         (define r (read))
         (cond [(fixnum? r) r]
               [else (error 'interp_exp "expected an integer" r)])]
        [(Prim '- (list e))
         (define v ((interp_exp env) e))
         (fx- 0 v)]
        [(Prim '+ (list e1 e2))
         (define v1 ((interp_exp env) e1))
         (define v2 ((interp_exp env) e2))
         (fx+ v1 v2)]
        [(Prim '- (list e1 e2))
         (define v1 ((interp_exp env) e1))
         (define v2 ((interp_exp env) e2))
         (fx- v1 v2)]
        ))

    (define/public (interp-program p)
      (match p
        [(Program '() e) ((interp_exp '()) e)]
        ))
    ))



