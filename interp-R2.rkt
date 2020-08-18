#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp-R2 interp-C1)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define primitives (set '+ '-  'read
                        'eq? '< '<= '> '>= 'not))

(define (interp-op op)
  (match op
    ['+ fx+]
    ['- fx-]
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
      [(Var x) (lookup x env)]
      [(Let x e body)
       (define new-env (cons (cons x ((interp-exp env) e)) env))
       ((interp-exp new-env) body)]
      [(Int n) n]
      [(Bool b) b]
      [(If cnd thn els)
       (define b (recur cnd))
       (match b
         [#t (recur thn)]
         [#f (recur els)])]
      [(Prim 'and (list e1 e2))
       (define v1 (recur e1))
       (match v1
         [#t (match (recur e2) [#t #t] [#f #f])]
         [#f #f])]
      [(Prim op args)
       (apply (interp-op op) (for/list ([e args]) (recur e)))]
      [else
       (error 'interp-exp "R2: unmatch" e)]
      )))

(define (interp-R2 p)
  (match p
    [(Program info e)
     ((interp-exp '()) e)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interp-C1-stmt env)
  (lambda (s)
    (match s
      [(Assign (Var x) e)
       (cons (cons x ((interp-exp env) e)) env)]
      [else
       (error "interp-C1-stmt unmatched" s)]
      )))

(define (interp-C1-tail env CFG)
  (lambda (t)
    (match t
      [(Return e)
       ((interp-exp env) e)]
      [(Goto l)
       ((interp-C1-tail env CFG) (dict-ref CFG l))]
      [(If (Prim op arg*) (Goto thn-label) (Goto els-label))
       (if ((interp-exp env) (Prim op arg*))
           ((interp-C1-tail env CFG) (dict-ref CFG thn-label))
           ((interp-C1-tail env CFG) (dict-ref CFG els-label)))]
      [(Seq s t2)
       (define new-env ((interp-C1-stmt env) s))
       ((interp-C1-tail new-env CFG) t2)]
      [else
       (error "interp-C1-tail unmatched" t)]
      )))
  
(define (interp-C1 p)
  (match p
    [(Program info (CFG G))
     ((interp-C1-tail '() G) (dict-ref G 'start))]
    ))
