#lang racket
(require "utilities.rkt")
(require "type-check-Lwhile.rkt")
(provide type-check-Lvec type-check-Lvec-has-type type-check-Lvec-class
         type-check-vec-mixin
         typed-vec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Tuples (aka Vectors)                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-vec-mixin

(define typed-vec (make-parameter #f))

(define (type-check-vec-mixin super-class)
  (class super-class
    (super-new)
    
    (inherit check-type-equal?)
    
    (define/override (type-equal? t1 t2)
      (debug 'type-equal? "lenient" t1 t2)
      (match* (t1 t2)
        [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
         (for/and ([t1 ts1] [t2 ts2])
           (type-equal? t1 t2))]
        [(other wise) (super type-equal? t1 t2)]))

    (define/override (type-check-exp env)
      (lambda (e)
        (define recur (type-check-exp env))
        (match e
          [(Prim 'vector es)
           (unless (<= (length es) 50)
             (error 'type-check "vector too large ~a, max is 50" (length es)))
           (define-values (e* t*) (for/lists (e* t*) ([e es]) (recur e)))
           (define t `(Vector ,@t*))
           (values (cond [(typed-vec) (HasType (Prim 'vector e*) t)]
                         [else (Prim 'vector e*)])
                   t)]
          [(Prim 'vector-ref (list e1 e2))
           (define-values (e1^ t) (recur e1))
           (match t
             [`(Vector ,ts ...)
              (match e2
                [(Int i)
                 (unless (and (0 . <= . i) (i . < . (length ts)))
                   (error 'type-check "index ~a out of bounds\nin ~v" i e))
                 (values (Prim 'vector-ref (list e1^ (Int i)))
                         (list-ref ts i))]
                [else
                 (error 'type-check
                        "expected constant index, not ~a" e2)])]
             [else (error 'type-check "expect Vector, not ~a\nin ~v" t e)])]
          [(Prim 'vector-set! (list e1 e2 arg) )
           (define-values (e-vec t-vec) (recur e1))
           (define-values (e-arg^ t-arg) (recur arg))
           (match t-vec
             [`(Vector ,ts ...)
              (match e2
                [(Int i)
                 (unless (and (0 . <= . i) (i . < . (length ts)))
                   (error 'type-check "index ~a out of bounds\nin ~v" i e))
                 (check-type-equal? (list-ref ts i) t-arg e)
                 (values (Prim 'vector-set! (list e-vec (Int i) e-arg^))
                         'Void)]
                [else
                 (error 'type-check
                        "expected constant index, not ~a" e2)])]
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
          [else ((super type-check-exp env) e)]
          )))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Lvec

(define type-check-Lvec-class (type-check-vec-mixin type-check-Lwhile-class))

(define (type-check-Lvec p)
  (send (new type-check-Lvec-class) type-check-program p))

(define (type-check-Lvec-has-type p)
  (typed-vec #t)
  (define result (send (new type-check-Lvec-class) type-check-program p))
  (typed-vec #f)
  result)



