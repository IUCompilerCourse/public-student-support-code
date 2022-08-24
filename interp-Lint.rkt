#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp-Lint interp-Lint-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter for Lint: integer arithmetic

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code. This code does not use
;;   the match 'app' feature because the book doesn't introduce
;;   that until a later.

(define (interp-exp e)
    (match e
      [(Int n) n]
      [(Prim 'read '())
       (define r (read))
       (cond [(fixnum? r) r]
             [else (error 'interp-exp "expected an integer" r)])]
      [(Prim '- (list e))
       (define v (interp-exp e))
       (fx- 0 v)]
      [(Prim '+ (list e1 e2))
       (define v1 (interp-exp e1))
       (define v2 (interp-exp e2))
       (fx+ v1 v2)]
      [(Prim '- (list e1 e2))
       (define v1 (interp-exp e1))
       (define v2 (interp-exp e2))
       (fx- v1 v2)]
      ))

(define (interp-Lint p)
  (match p
    [(Program '() e) (interp-exp e)]
    ))


;; This version of the interpreter for Lint is the base class
;; for interp-Rvar-class in interp-Rvar.rkt.

(define interp-Lint-class
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
        [(Prim '- (list e1 e2))
         (define v1 ((interp-exp env) e1))
         (define v2 ((interp-exp env) e2))
         (fx- v1 v2)]
        ))

    (define/public (interp-program p)
      (match p
        [(Program '() e) ((interp-exp '()) e)]
        ))
    ))



