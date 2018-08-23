#lang racket
(require racket/fixnum)
(require "utilities.rkt" (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-R7)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define get-tagged-type
  (lambda (e)
    (match e
      [`(tagged ,v ,ty) ty])))

(define valid-op?
  (lambda (op)
    (member op '(+ - and or not))))

(define interp-R7-op
  (lambda (op es)
    (match `(,op ,es)
      [`(+ ((tagged ,v1 Integer) (tagged ,v2 Integer)))
       `(tagged ,(fx+ v1 v2) Integer)]
      [`(- ((tagged ,v Integer)))
       `(tagged ,(fx- 0 v) Integer)]
      [`(and (,v1 ,v2))
       (match v1
         [`(tagged #f Boolean) v1]
         [else v2])]
      [`(or (,v1 ,v2))
       (match v1
         [`(tagged #f Boolean) v2]
         [else v1])]
      [`(not (,v1))
       (match v1
         [`(tagged #f Boolean) `(tagged #t Boolean)]
         [else `(tagged #f Boolean)])])))
         
(define (interp-R7 env)
  (lambda (ast)
    (vomit "interp-R7" ast env)
    (define recur (interp-R7 env))
    (match ast
      [(? symbol?) (lookup ast env)]
      [`(function-ref ,f) (lookup f env)]
      [`(function-ref ,f ,n) (lookup f env)] ;; This is to deal with the detail of our translation that it keeps the arity of functions in the funref 
      [(? integer?) `(tagged ,ast Integer)]
      [#t `(tagged #t Boolean)]
      [#f `(tagged #f Boolean)]
      [`(read) `(tagged ,(read-fixnum) Integer)]
      [`(lambda (,xs ...) ,body)
       `(tagged (lambda ,xs ,body ,env) (,@(map (lambda (x) 'Any) xs) -> Any))]
      [`(define (,f ,xs ...) ,body)
       (mcons f `(lambda ,xs ,body))]
      [`(program ,ds ... ,body)
       (let ([top-level (map (interp-R7 '()) ds)])
         ;; Use set-cdr! on define lambda's for mutual recursion
         (for/list ([b top-level])
           (set-mcdr! b (match (mcdr b)
                          [`(lambda ,xs ,body)
                           `(tagged (lambda ,xs ,body ,top-level) 
                                    (,@(map (lambda (x) 'Any) xs) -> Any))])))
         ((interp-R7 top-level) body))]
      [`(vector ,es ...)
       (let* ([elts (map recur es)]
              [tys (map get-tagged-type elts)])
         `(tagged ,(apply vector (map recur es)) (Vector ,@tys)))]
      [`(vector-set! ,(app recur e1^) ,(app recur n^) ,(app recur e2^))
       (match e1^ 
	 [`(tagged ,vec ,ty) 
	  (match n^
	    [`(tagged ,n ,ty)
	     (vector-set! vec n e2^)
	     `(tagged (void) Void)])])]
      [`(vector-ref ,(app recur e^) ,(app recur n^))
       (match e^ 
	 [`(tagged ,vec ,ty) 
	  (match n^
	    [`(tagged ,n ,ty)
	     (vector-ref vec n)])])]
      [`(let ([,x ,e]) ,body)
       (let ([v (recur e)])
         ((interp-R7 (cons (cons x v) env)) body))]
      [`(,op ,es ...) #:when (valid-op? op)
       (interp-R7-op op (map recur es))]
      [`(eq? ,l ,r)
       `(tagged ,(equal? (recur l) (recur r)) Boolean)]
      [`(if ,q ,t ,f)
       (match (recur q)
         [`(tagged #f Boolean)
          (recur f)]
         [else (recur t)])]
      [`(app ,f ,es ...) 
       (define new-args (map recur es))
       (let ([f-val (recur f)])
         (match f-val 
           [`(tagged (lambda (,xs ...) ,body ,lam-env) ,ty)
            (define new-env (append (map cons xs new-args) lam-env))
            ((interp-R7 new-env) body)]
           [else (error "interp-R7, expected function, not" f-val)]))]
      ;; The following case has to come last. -Jeremy
      [`(,f ,es ...)
       (define new-args (map recur es))
       (let ([f-val (recur f)])
         (match f-val 
           [`(tagged (lambda (,xs ...) ,body ,lam-env) ,ty)
            (define new-env (append (map cons xs new-args) lam-env))
            ((interp-R7 new-env) body)]
           [else (error "interp-R7, expected function, not" f-val)]))]
      )))


