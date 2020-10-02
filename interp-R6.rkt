#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "casts.rkt")
(provide interp-R6 interp-R6-prog)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define primitives (set '+ '- 'read
			'eq? '< '<= '> '>= 'not 'or
			'vector 'vector-ref 'vector-set!
			'boolean? 'integer? 'vector? 'procedure?
                        'tag-of-any 'value-of-any 'exit
                        'vector-proxy 'tag-of-vector))

(define (vector-ish? v)
  (match v
    [`(vector-proxy ,vec ,rs ,ws)
     (vector-ish? vec)]
    [else
     (vector? v)]))

;; The following should be moved to a new interpreter -Jeremy
#;(define (apply-vector-ref vec i)
  (match vec
    [`(vector-proxy ,v ,rs ,ws)
     (define v^ (apply-vector-ref v i))
     (define r (vector-ref rs i))
     (apply-fun r (list v^))]
    [else
     (vector-ref vec i)]))

;; The following should be moved to a new interpreter -Jeremy
#;(define (apply-vector-set! vec i arg)
  (match vec
    [`(vector-proxy ,v ,rs ,ws)
     (define w (vector-ref ws i))
     (define arg^ (apply-fun w (list arg)))
     (apply-vector-set! v i arg^)]
    [else
     (vector-set! vec i arg)]))


(define (interp-op op)
  (match op
    ['+ fx+]
    ['- fx-]
    ['read read-fixnum]
    ['not (lambda (v) (match v [#t #f] [#f #t]))]
    ['or (lambda (v1 v2)
           (cond [(and (boolean? v1) (boolean? v2))
                  (or v1 v2)]))]
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
    ;; The following should be moved to a new interpreter -Jeremy
    ['vector-proxy (lambda (vec rs ws) `(vector-proxy ,vec ,rs ,ws))]
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
                  [`(tagged ,v1 (Vectorof ,t)) #t]
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
			  (and (vector-ish? v1) (vector-ish? v2))
                          (and (void? v1) (void? v2)))
		      (eq? v1 v2)]
                     [else
                      (error 'interp-R6 "unrelated values in eq? ~a ~a" v1 v2)]
                     )]))]
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

(define (apply-fun fun-val arg-vals)
  (match fun-val
    [`(lambda (,xs ...) ,body ,lam-env)
     (define new-env (append (map cons xs arg-vals) lam-env))
     ((interp-exp new-env) body)]
    [else (error "interp-exp, expected function, not" fun-val)]))

(define (interp-exp env)
  (lambda (e)
    (define recur (interp-exp env))
    (verbose "R6/interp-exp" e)
    (let ([ret
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
                [#f (recur els)]
                [else(error 'inter-exp "R6, expected Boolean condition, not ~a" b)]
                )]
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
              (apply-inject ((interp-exp env) e) t)]
             [`(project ,e ,t2)
              (define v (recur e))
              (apply-project v t2)]
             [`(tag-of-vector ,e)
              (define v (recur e))
              (match v
                [`(vector-proxy ,vec ,rs ,ws) 1]
                [else 0])]
             [`(value-of-any ,e ,ty)
              ((interp-op 'value-of-any) (recur e))]
             [`(,op ,args ...)
              #:when (set-member? primitives op)
              (apply (interp-op op) (map recur args))]
             ;; The following case has to come last. -Jeremy
             [(or `(app ,fun ,args ...) `(,fun ,args ...))
              (define fun-val ((interp-exp env) fun))
              (define arg-vals (map (interp-exp env) args))
              (apply-fun fun-val arg-vals)]
             [else (error 'interp-exp "unrecognized expression")]
             )])
      (verbose "R6/interp-exp ==>" ret)
      ret)))

(define (interp-def env)
  (lambda (d)
    (match d
      [(or `(define (,f [,xs : ,ps] ...) : ,rt ,body)
           `(define (,f [,xs : ,ps] ...) : ,rt ,_ ,body))
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

(define (interp-R6 p)
  (match p
    [`(program ,info ,defs ... ,body)
     (let ([top-level (map (interp-def '()) defs)])
       (for/list ([b top-level])
         (set-mcdr! b (match (mcdr b)
                        [`(lambda ,xs ,body)
                         `(lambda ,xs ,body ,top-level)])))
       ((interp-exp top-level) body))]
    ))
