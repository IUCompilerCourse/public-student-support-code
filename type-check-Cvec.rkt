#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Cif.rkt")
(require "type-check-Cwhile.rkt")
(require "type-check-Lvec.rkt")
(provide type-check-Cvec type-check-Cvec-mixin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cvec

(define (type-check-Cvec-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal? exp-ready?)

    (define/override (type-equal? t1 t2)
      (debug 'type-equal? "lenient" t1 t2)
      (match* (t1 t2)
        [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
         (for/and ([t1 ts1] [t2 ts2])
           (type-equal? t1 t2))]
        [(other wise) (super type-equal? t1 t2)]))
    
    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(HasType e t) (recur e)]
        [(Allocate size ty) (set)]
        [(GlobalValue name) (set)]
        [else (super free-vars-exp e)]))
        
    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "Cvec" e)
        (define recur (type-check-exp env))
        (match e
          [(Prim 'vector es)
           (unless (<= (length es) 50)
             (error 'type-check "vector too large ~a, max is 50" (length es)))
           (define-values (e* t*) (for/lists (e* t*) ([e es]) (recur e)))
           (define t `(Vector ,@t*))
           (values (HasType (Prim 'vector e*) t)  t)]
          [(Prim 'vector-ref (list e1 (Int i)))
           (define-values (e1^ t) (recur e1))
           (match t
             [`(Vector ,ts ...)
              (unless (and (0 . <= . i) (i . < . (length ts)))
                (error 'type-check "index ~a out of bounds\nin ~v" i e))
              (values (Prim 'vector-ref (list e1^ (Int i)))  (list-ref ts i))]
             [else (error 'type-check "expect Vector, not ~a\nin ~v" t e)])]
          [(Prim 'vector-set! (list e1 (Int i) arg) )
           (define-values (e-vec t-vec) (recur e1))
           (define-values (e-arg^ t-arg) (recur arg))
           (match t-vec
             [`(Vector ,ts ...)
              (unless (and (0 . <= . i) (i . < . (length ts)))
                (error 'type-check "index ~a out of bounds\nin ~v" i e))
              (check-type-equal? (list-ref ts i) t-arg e)
              (values (Prim 'vector-set! (list e-vec (Int i) e-arg^))  'Void)]
             [else (error 'type-check "expect Vector, not ~a\nin ~v" t-vec e)])]
          [(Prim 'vector-length (list e))
           (define-values (e^ t) (recur e))
           (match t
             [`(Vector ,ts ...)
              (values (Prim 'vector-length (list e^))  'Integer)]
             [else (error 'type-check "expect Vector, not ~a\nin ~v" t e)])]
          [(Prim 'eq? (list arg1 arg2))
           (define-values (e1 t1) (recur arg1))
           (define-values (e2 t2) (recur arg2))
           (match* (t1 t2)
             [(`(Vector ,ts1 ...)  `(Vector ,ts2 ...))  (void)]
             [(other wise)  (check-type-equal? t1 t2 e)])
           (values (Prim 'eq? (list e1 e2)) 'Boolean)]
          [(GlobalValue name)
           (values (GlobalValue name) 'Integer)]
          [(Allocate size t)
           (values (Allocate size t) t)]
          [(Collect size)
           (values (Collect size) 'Void)]
          [else ((super type-check-exp env) e)])))
    
    (define/override ((type-check-stmt env) s)
      (match s
        [(Collect size) (void)]
        [(Prim 'vector-set! (list vec index rhs))
         #:when (and (exp-ready? vec env) (exp-ready? index env)
                     (exp-ready? rhs env))
         ((type-check-exp env) s)]
        [else ((super type-check-stmt env) s)]))
    
    ))

(define type-check-Cvec-class (type-check-Cvec-mixin
                               (type-check-Cwhile-mixin
                                (type-check-Cif-mixin
                                 (type-check-Cvar-mixin
                                  type-check-Lvec-class)))))

(define (type-check-Cvec p)
  (send (new type-check-Cvec-class) type-check-program p))


  
