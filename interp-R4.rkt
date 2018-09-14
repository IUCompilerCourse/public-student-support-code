#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp-R4)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define primitives (set '+ '- 'read
                        'eq? '< '<= '> '>= 'not 
                        'vector 'vector-ref 'vector-set!))

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
    ['vector vector]
    ['vector-ref vector-ref]
    ['vector-set! vector-set!]
    [else (error 'interp-op "unknown operator")]
    ))

(define (interp-exp env)
  (lambda (e)
    (define recur (interp-exp env))
    (verbose "R4/interp-exp" e)
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
      [`(void) (void)]
      [`(,op ,(app recur args) ...)
       #:when (set-member? primitives op)
       (apply (interp-op op) args)]
      [`(,fun ,args ...)
       (define arg-vals (map (interp-exp env) args))
       (define fun-val ((interp-exp env) fun))
       (match fun-val
	 [`(lambda (,xs ...) ,body)
	  (define new-env (append (map cons xs arg-vals) env))
	  ((interp-exp new-env) body)]
	 [else (error "interp-exp, expected function, not" fun-val)])]
      [else (error 'interp-exp "unrecognized expression")]
      )))

(define (interp-def env)
  (lambda (d)
    (match d
      [`(define (,f [,xs : ,ps] ...) : ,rt ,body)
       (cons f `(lambda ,xs ,body))]
      )))

(define (interp-R4 env)
  (lambda (p)
    (verbose "R4/interp-R4" p)
    (match p
      ;; This first variant is needed after type checking
      [(or `(program (type ,_) ,ds ... ,body)
           `(program ,ds ... ,body))
       (let ([top-level (map  (interp-def '()) ds)])
	 ((interp-exp top-level) body))]
      )))
