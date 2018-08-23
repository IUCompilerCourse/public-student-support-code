#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp-R1)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define (interp-exp env)
  (lambda (e)
    (match e
      [(? symbol?) (lookup e env)]
      [`(let ([,x ,(app (interp-exp env) v)]) ,body)
       (define new-env (cons (cons x v) env))
       ((interp-exp new-env) body)]
      [(? fixnum?) e]
      [`(read)
       (define r (read))
       (cond [(fixnum? r) r]
             [else (error 'interp-R1 "expected an integer" r)])]
      [`(- ,(app (interp-exp env) v))
       (fx- 0 v)]
      [`(+ ,(app (interp-exp env) v1) ,(app (interp-exp env) v2))
       (fx+ v1 v2)]
      )))

(define (interp-R1 env)
  (lambda (p)
    (match p
      [`(program ,e) ((interp-exp '()) e)])))
