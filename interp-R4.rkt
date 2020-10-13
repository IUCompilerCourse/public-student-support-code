#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp-R4)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define primitives (set '+ '- 'read
                        'eq? '< '<= '> '>= 'not 'or
                        'vector 'vector-ref 'vector-set!))

(define (interp-op op)
  (match op
    ['+ fx+]
    ['- fx-]
    ['read read-fixnum]
    ['not (lambda (v) (match v [#t #f] [#f #t]))]
    ['or (lambda (v1 v2)
           (cond [(and (boolean? v1) (boolean? v2))
                  (or v1 v2)]))]
    ['eq? (lambda (v1 v2)
	    (cond [(or (and (fixnum? v1) (fixnum? v2))
		       (and (boolean? v1) (boolean? v2))
		       (and (vector? v1) (vector? v2))
                       (and (void? v1) (void? v2)))
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
      [(HasType e t)
       (recur e)]
      [(Void) (void)]
      [(Prim op args)
       #:when (set-member? primitives op)
       (apply (interp-op op) (for/list ([e args]) (recur e)))]
      [(Apply fun args)
       (define fun-val (recur fun))
       (define arg-vals (for/list ([e args]) (recur e)))
       (match fun-val
	 [`(lambda (,xs ...) ,body ,fun-env)
	  (define new-env (append (map cons xs arg-vals) fun-env))
	  ((interp-exp new-env) body)]
	 [else (error "interp-exp, expected function, not ~a" fun-val)])]
      [else (error 'interp-exp "unrecognized expression ~a" e)]
      )))

(define (interp-def d)
  (match d
    [(Def f (list `[,xs : ,ps] ...) rt _ body)
     (mcons f `(lambda ,xs ,body ()))]
    ))


(define (interp-R4 p)
  (verbose "interp-R4" p)
  (match p
    [(ProgramDefsExp info ds body)
     (let ([top-level (for/list ([d ds]) (interp-def d))])
       (for/list ([b top-level])
         (set-mcdr! b (match (mcdr b)
                        [`(lambda ,xs ,body ())
                         `(lambda ,xs ,body ,top-level)])))
       ((interp-exp top-level) body))]
    
    ;; For after the shrink pass.
    [(ProgramDefs info ds)
     (let ([top-level (for/list ([d ds]) (interp-def d))])
       (for ([b top-level])
         (set-mcdr! b (match (mcdr b)
                        [`(lambda ,xs ,body ())
                         `(lambda ,xs ,body ,top-level)])))
       ;; call the main function
       ((interp-exp top-level) (Apply (Var 'main) '())))]
    ))
