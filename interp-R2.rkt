#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp-R2)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define primitives (set '+ '-  'read
                        'eq? '< '<= '> '>= 'not))

(define (interp-op op)
  (match op
    ['+ fx+]
    ['- (lambda (n) (fx- 0 n))]
    ['read read-fixnum]
    ['not (lambda (v) (match v [#t #f] [#f #t]))]
    ['eq? (lambda (v1 v2)
	    (cond [(or (and (fixnum? v1) (fixnum? v2))
		       (and (boolean? v1) (boolean? v2))
		       (and (vector? v1) (vector? v2)))
		   (eq? v1 v2)]))]
    ['< (lambda (v1 v2)
	  (cond [(and (fixnum? v1) (fixnum? v2))
		 (< v1 v2)]))]
    ['<= (lambda (v1 v2)
	   (cond [(and (fixnum? v1) (fixnum? v2))
		  (<= v1 v2)]))]
    ['> (lambda (v1 v2)
	  (cond [(and (fixnum? v1) (fixnum? v2))
		 (> v1 v2)]))]
    ['>= (lambda (v1 v2)
	   (cond [(and (fixnum? v1) (fixnum? v2))
		  (>= v1 v2)]))]
    [else (error 'interp-op "unknown operator")]
    ))

(define (interp-exp env)
  (lambda (e)
    (define recur (interp-exp env))
    (match e
      [(? symbol?) (lookup e env)]
      [`(let ([,x ,(app (interp-exp env) v)]) ,body)
       (define new-env (cons (cons x v) env))
       ((interp-exp new-env) body)]
      [(? fixnum?) e]
      [(? boolean?) e]
      [`(if ,(app recur cnd) ,thn ,els)
       (match cnd
	      [#t (recur thn)]
	      [#f (recur els)])]
      [`(and ,(app recur v1) ,e2)
       (match v1
	      [#t (match (recur e2) [#t #t] [#f #f])]
	      [#f #f])]
      [`(has-type ,(app recur v) ,t)
       v]
      [`(,op ,(app recur args) ...)
       #:when (set-member? primitives op)
       (apply (interp-op op) args)]
      )))

(define (interp-R2 env)
  (lambda (p)
    (match p
      ;; the first variant is needed after type checking
      [(or `(program ,_ ,e) `(program ,e))
       ((interp-exp '()) e)]
      )))
