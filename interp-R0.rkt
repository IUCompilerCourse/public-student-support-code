#lang racket
(require racket/fixnum)
(provide interp-R0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter for R0: integer arithmetic

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code. This code does not use
;;   the match 'app' feature because the book doesn't introduce
;;   that until a later.

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
      )))

(define (interp-R0 p)
  (match p
    [(Program info e) ((interp-exp '()) e)]
    ))


