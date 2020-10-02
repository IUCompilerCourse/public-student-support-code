#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp-R0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter for R0: integer arithmetic

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
             [else (error 'interp-R1 "expected an integer" r)])]
      [(Prim '- (list e))
       (define v (interp-exp e))
       (fx- 0 v)]
      [(Prim '+ (list e1 e2))
       (define v1 (interp-exp e1))
       (define v2 (interp-exp e2))
       (fx+ v1 v2)]
      ))

(define (interp-R0 p)
  (match p
    [(Program '() e) (interp-exp e)]
    ))


