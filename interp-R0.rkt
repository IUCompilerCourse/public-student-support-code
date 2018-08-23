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

(define (interp-exp e)
  (match e
    [(? fixnum?) e]
    [`(read)
     (let ([r (read)])
       (cond [(fixnum? r) r]
             [else (error 'interp-R0 "input not an integer" r)]))]
    [`(- ,e1)     (fx- 0 (interp-exp e1))]
    [`(+ ,e1 ,e2) (fx+ (interp-exp e1) (interp-exp e2))]))

(define (interp-R0 p)
  (match p
    [`(program ,e) (interp-exp e)]))

