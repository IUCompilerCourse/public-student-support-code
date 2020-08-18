#lang racket
(require racket/fixnum)
(require "utilities.rkt" (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-R7 interp-R7-prog)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define (tag-of-any e)
  (match e
    [`(tagged ,v ,ty) ty]
    [else (error 'tag-of-any "expected a tagged value, not ~a" e)]
    ))

(define (value-of-any e)
  (match e
    [`(tagged ,v ,ty) v]
    [else (error 'value-of-any "expected a tagged value, not ~a" e)]))

(define primitives (set '+ '- 'read
			'< '<= '> '>= 'not
			'vector 'vector-ref 'vector-set!
			'boolean? 'integer? 'vector? 'procedure?))

(define (interp-op op)
  (match op
    ['+ fx+]
    ['- fx-]
    ['read read-fixnum]
    ['not (lambda (v) (match v [#t #f] [#f #t]))]
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
    [else (error 'interp-op "unknown operator")]
    ))

(define (tag-value v)
  (cond [(boolean? v) `(tagged ,v Boolean)]
        [(fixnum? v) `(tagged ,v Integer)]
        [(procedure? v)
         (define n (procedure-arity v))
         `(tagged ,v (,@(for/list ([_ (range 0 n)]) 'Any) -> Any))]
        [(vector? v) `(tagged ,v (Vectorof Any))]
        [(void? v) `(tagged ,v Void)]
        [else
         (error 'tag-value "unidentified value ~a" v)]
        ))

(define (interp-R7-exp env)
  (lambda (ast)
    (vomit "interp-R7-exp" ast env)
    (define recur (interp-R7-exp env))
    (match ast
      [(? symbol?) (lookup ast env)]
      [`(fun-ref ,f) (lookup f env)]
      [`(fun-ref ,f ,n) (lookup f env)] ;; This is to deal with the detail of our translation that it keeps the arity of functions in the funref 
      [(? integer?) `(tagged ,ast Integer)]
      [#t `(tagged #t Boolean)]
      [#f `(tagged #f Boolean)]
      [`(read) `(tagged ,(read-fixnum) Integer)]
      [`(lambda (,xs ...) ,body)
       `(tagged (lambda ,xs ,body ,env)
                (,@(for/list ([x xs]) 'Any) -> Any))]
      [`(vector ,es ...)
       `(tagged ,(apply vector (for/list ([e es]) (recur e)))
                (Vector ,@(for/list ([e es]) 'Any)))]
      [`(vector-set! ,e1 ,n ,e2)
       (define vec (value-of-any (recur e1)))
       (define i (value-of-any (recur n)))
       (vector-set! vec i (recur e2))
       `(tagged (void) Void)]
      [`(vector-ref ,e1 ,n)
       (define vec (value-of-any (recur e1)))
       (define i (value-of-any (recur n)))
       (vector-ref vec i)]
      [`(let ([,x ,e]) ,body)
       (let ([v (recur e)])
         ((interp-R7-exp (cons (cons x v) env)) body))]
      [`(and ,e1 ,e2)
       (recur `(if ,e1 ,e2 #f))]
      [`(or ,e1 ,e2)
       (define tmp (gensym 'tmp))
       (recur `(let ([,tmp ,e1]) (if ,tmp ,tmp ,e2)))]
      [`(eq? ,l ,r)
       `(tagged ,(equal? (recur l) (recur r)) Boolean)]
      [`(if ,q ,t ,f)
       (match (value-of-any (recur q))
         [#f (recur f)]
         [else (recur t)])]
      [`(,op ,es ...)
       #:when (set-member? primitives op)
       (tag-value
        (apply (interp-op op) (for/list ([e es]) (value-of-any (recur e)))))]
      ;; The following case has to come last. -Jeremy
      [(or `(app ,f ,es ...) `(,f ,es ...))
       (define new-args (map recur es))
       (let ([f-val (value-of-any (recur f))])
         (match f-val 
           [`(lambda (,xs ...) ,body ,lam-env)
            (define new-env (append (map cons xs new-args) lam-env))
            ((interp-R7-exp new-env) body)]
           [else (error "interp-R7-exp, expected function, not" f-val)]))]
      )))

(define (interp-R7-def ast)
  (vomit "interp-R7-def" ast)
  (match ast
    [(or `(define (,f ,xs ...) ,body) `(define (,f ,xs ...) ,_ ,body))
     (mcons f `(lambda ,xs ,body ()))]
    [else
     (error "interp-R7-def unmatched" ast)]
    ))

;; This version is for source code in R7.
(define (interp-R7 ast)
  (vomit "interp-R7" ast)
  (match ast
    [`(program ,info ,ds ... ,body)
     (let ([top-level (map (lambda (d) (interp-R7-def d)) ds)])
         ;; Use set-cdr! on define lambda's for mutual recursion
       (for/list ([b top-level])
         (set-mcdr! b (match (mcdr b)
                        [`(lambda ,xs ,body ())
                         `(tagged (lambda ,xs ,body ,top-level) 
                                  (,@(map (lambda (x) 'Any) xs) -> Any))])))
       (match ((interp-R7-exp top-level) body)
         [`(tagged ,n Integer)
          n]
         [v
          (error 'interp-R7 "expected an integer result from the program, not " v)]))]
    [else
     (error "interp-R7 unmatched" ast)]
    ))

;; This version is for after uniquify.
(define (interp-R7-prog ast)
  (vomit "interp-R7-prog" ast)
  (match ast
    [`(program ,info ,ds ...)
     (let ([top-level (map (lambda (d) (interp-R7-def d)) ds)])
       ;; Use set-cdr! on define lambda's for mutual recursion
       (for/list ([b top-level])
         (set-mcdr! b (match (mcdr b)
                        [`(lambda ,xs ,body ())
                         `(tagged (lambda ,xs ,body ,top-level) 
                                  (,@(map (lambda (x) 'Any) xs) -> Any))])))
       (match ((interp-R7-exp top-level) `(main))
         [`(tagged ,n Integer)
          n]
         [v
          (error 'interp-R7 "expected an integer result from the program, not " v)]))]
    [else
     (error "interp-R7-prog unmatched" ast)]
    ))
