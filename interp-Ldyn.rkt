#lang racket
(require racket/fixnum)
(require "utilities.rkt" (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Ldyn interp-Ldyn-prog)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

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
    ['boolean? boolean?]
    ['integer? fixnum?]
    ['void? void?]
    ['vector? vector?]
    ['vector-length vector-length]
    ['procedure? (match-lambda [(Function xs body env) #t]
                               [else #f])]
    [else (error 'interp-op "unknown operator ~a" op)]))

(define (op-tags op)
  (match op
    ['+ '((Integer Integer))]
    ['- '((Integer Integer) (Integer))]
    ['read '(())]
    ['not '((Boolean))]
    ['< '((Integer Integer))]
    ['<= '((Integer Integer))]
    ['> '((Integer Integer))]
    ['>= '((Integer Integer))]
    ['vector-length '((Vector))]
    ))

(define type-predicates
  (set 'boolean? 'integer? 'vector? 'procedure? 'void?))

(define (tag-value v)
  (cond [(boolean? v) (Tagged v 'Boolean)]
        [(fixnum? v) (Tagged v 'Integer)]
        [(procedure? v) (Tagged v 'Procedure)]
        [(vector? v) (Tagged v 'Vector)]
        [(void? v) (Tagged v 'Void)]
        [else (error 'tag-value "unidentified value ~a" v)]))

(define (check-tag val expected ast)
  (define tag (Tagged-tag val))
  (unless (eq? tag expected)
    (error 'trapped-error "expected ~a tag, not ~a\nin ~v" expected tag ast)))

(define ((interp-Ldyn-exp env) ast)
  (verbose 'interp-Ldyn "start" ast)
  (define recur (interp-Ldyn-exp env))
  (define result
    (match ast
      [(Var x) (unbox (lookup x env))]
      [(FunRef f n) (unbox (lookup f env))]
      [(Int n) (Tagged n 'Integer)]
      [(Bool b) (Tagged b 'Boolean)]
      [(Lambda xs rt body)
       (Tagged (Function xs body env) 'Procedure)]
      [(Prim 'vector es)
       (Tagged (apply vector (for/list ([e es]) (recur e))) 'Vector)]
      [(Prim 'vector-ref (list e1 e2))
       (define vec (recur e1)) (define i (recur e2))
       (check-tag vec 'Vector ast) (check-tag i 'Integer ast)
       (unless (< (Tagged-value i) (vector-length (Tagged-value vec)))
         (error 'trapped-error "index ~a too big\nin ~v" (Tagged-value i) ast))
       (vector-ref (Tagged-value vec) (Tagged-value i))]
      [(Prim 'vector-set! (list e1 e2 e3))
       (define vec (recur e1)) (define i (recur e2)) (define arg (recur e3))
       (check-tag vec 'Vector ast) (check-tag i 'Integer ast)
       (unless (< (Tagged-value i) (vector-length (Tagged-value vec)))
         (error 'trapped-error "index ~a too big\nin ~v" (Tagged-value i) ast))
       (vector-set! (Tagged-value vec) (Tagged-value i) arg)
       (Tagged (void) 'Void)]
      [(Let x e body)
       ((interp-Ldyn-exp (cons (cons x (box (recur e))) env)) body)]
      [(Prim 'and (list e1 e2)) (recur (If e1 e2 (Bool #f)))]
      [(Prim 'or (list e1 e2))
       (define v1 (recur e1))
       (match (Tagged-value v1) [#f (recur e2)] [else v1])]
      [(Prim 'not (list e1))
       (match (Tagged-value (recur e1)) [#f (Tagged #t 'Boolean)]
              [else (Tagged #f 'Boolean)])]
      [(Prim 'eq? (list e1 e2))
       (Tagged (equal? (recur e1) (recur e2)) 'Boolean)]
      [(Prim op (list e1))
       #:when (set-member? type-predicates op)
       (tag-value ((interp-op op) (Tagged-value (recur e1))))]
      [(Prim op es)
       (define args (map recur es))
       (define tags (for/list ([arg args]) (Tagged-tag arg)))
       (unless (for/or ([expected-tags (op-tags op)])
                 (equal? expected-tags tags))
         (error 'trapped-error "illegal argument tags ~a\nin ~v" tags ast))
       (tag-value
        (apply (interp-op op) (for/list ([a args]) (Tagged-value a))))]
      [(If q t f)
       (match (Tagged-value (recur q)) [#f (recur f)] [else (recur t)])]
      [(GetBang x) (unbox (lookup x env))]
      [(SetBang x rhs)
       (set-box! (lookup x env) (recur rhs))]
      [(Begin es body)
       (for ([e es]) (recur e))
       (recur body)]
      [(WhileLoop cnd body)
       (define (loop)
	 (match (Tagged-value (recur cnd))
		[#f (tag-value (void))]
		[else (loop)]))
       (loop)]
      [(Void)  (tag-value (void))]
      [(Apply f es)
       (define new-f (recur f))
       (define args (map (lambda (arg) (box (recur arg))) es))
       (check-tag new-f 'Procedure ast)
       (define f-val (Tagged-value new-f))
       (match f-val 
         [(Function xs body lam-env)
          (unless (eq? (length xs) (length args))
            (error 'trapped-error "number of arguments ~a != arity ~a\nin ~v"
                   (length args) (length xs) ast))
          (define new-env (append (map cons xs args) lam-env))
          ((interp-Ldyn-exp new-env) body)]
         [else (error "interp-Ldyn-exp, expected function, not" f-val)])]))
  (verbose 'interp-Ldyn ast result)
  result)

(define (interp-Ldyn-def ast)
  (match ast
    [(Def f xs rt info body) (mcons f (Function xs body '()))]))

;; This version is for source code in Ldyn.
(define (interp-Ldyn ast)
  (match ast
    [(ProgramDefsExp info ds body)
     (define top-level (map (lambda (d) (interp-Ldyn-def d)) ds))
     (for/list ([b top-level])
       (set-mcdr! b (match (mcdr b)
                      [(Function xs body '())
                       (box (Tagged (Function xs body top-level) 'Procedure))])))
     (define result ((interp-Ldyn-exp top-level) body))
     (check-tag result 'Integer ast)
     (Tagged-value result)]
    [(Program info body) (interp-Ldyn (ProgramDefsExp info '() body))]))

;; This version is for after shrink.
(define (interp-Ldyn-prog ast)
  (match ast
    [(ProgramDefs info ds)
     (define top-level (map (lambda (d) (interp-Ldyn-def d)) ds))
     (for/list ([b top-level])
       (set-mcdr! b (match (mcdr b)
                      [(Function xs body '())
                       (box (Tagged (Function xs body top-level) 'Procedure))])))
     (define result ((interp-Ldyn-exp top-level) (Apply (Var 'main) '())))
     (check-tag result 'Integer ast)
     (Tagged-value result)]))
