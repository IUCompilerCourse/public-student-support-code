#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp-R6 interp-R6-prog)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define primitives (set '+ '- 'read
			'eq? '< '<= '> '>= 'not
			'vector 'vector-ref 'vector-set!
			'boolean? 'integer? 'vector? 'procedure? 'eq? 'tag-of-any 'value-of-any 'exit))

(define (interp-op op)
  (match op
    ['+ fx+]
    ['- (lambda (n) (fx- 0 n))]
    ['read read-fixnum]
    ['not (lambda (v) (match v [#t #f] [#f #t]))]
    ['eq? (lambda (v1 v2)
	    (cond [(or (and (fixnum? v1) (fixnum? v2))
		       (and (boolean? v1) (boolean? v2))
		       (and (vector? v1) (vector? v2))
                       (and (void? v1) (void? v2)))
		   (eq? v1 v2)]
                  [else (error "R6/interp-op bad arg to eq?" v1 v2)]))]
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
    ['boolean? (lambda (v)
		 (match v
		   [`(tagged ,v1 Boolean) #t]
		   [else #f]))]
    ['integer? (lambda (v)
		 (match v
		   [`(tagged ,v1 Integer) #t]
		   [else #f]))]
    ['vector? (lambda (v)
		(match v
		  [`(tagged ,v1 (Vector ,ts ...)) #t]
		  [else #f]))]
    ['procedure? (lambda (v)
		   (match v
		     [`(tagged ,v1 (,ts ... -> ,rt)) #t]
		     [else #f]))]
    ['eq? (lambda (v1 v2)
	    (match (list v1 v2)
 	      [`((tagged ,v1^ ,ty1) (tagged ,v2^ ,ty2))
	       (and (eq? v1^ v2^) (equal? ty1 ty2))]
	      [else
	       (cond [(or (and (fixnum? v1) (fixnum? v2))
			  (and (boolean? v1) (boolean? v2))
			  (and (vector? v1) (vector? v2)))
		      (eq? v1 v2)])]))]
    ['tag-of-any (lambda (v)
                   (match v
                     [`(tagged ,v^ ,ty)
                      (any-tag ty)]
                     [else
                      (error "interp-R6 expected tagged value, not" v)]))]
    ['value-of-any (lambda (v)
                     (match v
                       [`(tagged ,v^ ,ty)
                        v^]
                       [else
                        (error "interp-R6 expected tagged value, not" v)]))]
    [else (error 'interp-op "unknown operator")]
    ))

;; Equality for flat types.
(define (tyeq? t1 t2)
  (match `(,t1 ,t2)
    [`((Vectorof Any) (Vector ,t2s ...))
     (for/and ([t2 t2s])
       (eq? t2 'Any))]
    [`((Vector ,t1s ...) (Vectorof Any))
     (for/and ([t1 t1s])
       (eq? t1 'Any))]
    [else (equal? t1 t2)]))

(define (interp-exp env)
  (lambda (e)
    (define recur (interp-exp env))
    (match e
      [(? symbol?) (lookup e env)]
      [`(let ([,x ,e]) ,body)
       (define new-env (cons (cons x ((interp-exp env) e)) env))
       ((interp-exp new-env) body)]
      [(? fixnum?) e]
      [(? boolean?) e]
      [`(if ,cnd ,thn ,els)
       (define b (recur cnd))
       (match b
         [#t (recur thn)]
         [#f (recur els)])]
      [`(and ,e1 ,e2)
       (define v1 (recur e1))
       (match v1
         [#t (match (recur e2) [#t #t] [#f #f])]
         [#f #f])]
      [`(has-type ,e ,t)
       (recur e)]
      [`(void) (void)]
      [`(lambda: ([,xs : ,Ts] ...) : ,rT ,body)
       `(lambda ,xs ,body ,env)]
      [`(inject ,e ,t)
       `(tagged ,((interp-exp env) e) ,t)]
      [`(project ,e ,t2)
       (define v ((interp-exp env) e))
       (match v
         [`(tagged ,v1 ,t1)
	  (cond [(tyeq? t1 t2)
		 v1]
		[else
		 (error "in project, type mismatch" t1 t2)])]
	 [else
	  (error "in project, expected tagged value" v)])]
      [`(value-of-any ,e ,ty)
       ((interp-op 'value-of-any) (recur e))]
      ;; The following two cases have to come last. -Jeremy
      [`(,op ,args ...)
       #:when (set-member? primitives op)
       (apply (interp-op op) (map recur args))]
      [`(,fun ,args ...)
       (define fun-val ((interp-exp env) fun))
       (define arg-vals (map (interp-exp env) args))
       (match fun-val
	 [`(lambda (,xs ...) ,body ,lam-env)
	  (define new-env (append (map cons xs arg-vals) lam-env))
	  ((interp-exp new-env) body)]
	 [else (error "interp-exp, expected function, not" fun-val)])]
      [else (error 'interp-exp "unrecognized expression")]
      )))

(define (interp-def env)
  (lambda (d)
    (match d
      [`(define (,f [,xs : ,ps] ...) : ,rt ,info ,body)
       (mcons f `(lambda ,xs ,body))]
      [else
       (error "R6/interp-def unmatched" d)]
      )))

(define (interp-R6-prog p)
  (match p
    [`(program ,info ,defs ...)
    (let ([top-level (map (interp-def '()) defs)])
      (for/list ([b top-level])
        (set-mcdr! b (match (mcdr b)
                       [`(lambda ,xs ,body)
                        `(lambda ,xs ,body ,top-level)])))
      ((interp-exp top-level) `(main)))]
    ))

(define (interp-R6 env)
  (lambda (p)
    (match p
      [(or `(program (type ,_) ,defs ... ,body)
	   `(program ,defs ... ,body))
       (let ([top-level (map (interp-def '()) defs)])
	 (for/list ([b top-level])
		   (set-mcdr! b (match (mcdr b)
				  [`(lambda ,xs ,body)
				   `(lambda ,xs ,body ,top-level)])))
	 ((interp-exp top-level) body))]
      )))
