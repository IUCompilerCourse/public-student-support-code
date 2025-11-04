#lang racket
(require "utilities.rkt")
(require "type-check-Lvec.rkt")
(provide type-check-Lvecof type-check-Lvecof-has-type
         type-check-Lvecof-class type-check-vecof-mixin typed-vecof)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Homogeneous Vectors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Lvecof

(define typed-vecof (make-parameter #f))

(define (type-check-vecof-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal?)

    (define/override (operator-types)
      (append '((* . ((Integer Integer) . Integer))
                (exit . (() . _)))
              (super operator-types)))

    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "vecof" e)
        (define recur (type-check-exp env))
        (match e
          [(Prim 'make-vector (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ elt-type) (recur e2))
           (define vec-type `(Vectorof ,elt-type))
           (define mk-vec (Prim 'make-vector (list e1^ e2^)))
           (values (cond [(typed-vecof) (HasType mk-vec vec-type)]
                         [else mk-vec])
                   vec-type)]
          
          [(Prim 'vector-ref (list e1 e2))
           (define-values (e1^ t) (recur e1))
           (match t
             [`(Vectorof ,elt-type)
              (define-values (e2^ t2) (recur e2))
              (check-type-equal? t2 'Integer e2)
              (values (Prim 'vector-ref (list e1^ e2^)) elt-type)]
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
             [`(Vectorof ,elt-type)
              (define-values (e2^ t2) (recur e2))
              (check-type-equal? t2 'Integer e2)
              (check-type-equal? elt-type t-arg e)
              (values (Prim 'vector-set! (list e-vec e2^ e-arg^))  'Void)]
             
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
             [`(Vectorof ,elt-type)
              (values (Prim 'vector-length (list e^))  'Integer)]

             [`(Vector ,ts ...)
              (values (Prim 'vector-length (list e^))  'Integer)]
             [else (error 'type-check "expect Vector, not ~a\nin ~v" t e)])]
          
          [(Prim 'vectorof-ref (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (check-type-equal? t2 'Integer e2)
           (match t1
             [`(Vectorof ,elt-type)
              (values (Prim 'vectorof-ref (list e1^ e2^)) elt-type)]
             [else
              (error 'type-check
                     "expected a vectorof in vectorof-ref, not " t1)])]
          
          [(Prim 'vectorof-set! (list e1 e2 e3) )
           (define-values (e-vec t-vec) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (define-values (e-arg^ t-arg) (recur e3))
           (check-type-equal? t2 'Integer e2)
           (match t-vec
             [`(Vectorof ,elt-type)
              (check-type-equal? elt-type t-arg e)
              (values (Prim 'vectorof-set! (list e-vec e2^ e-arg^))  'Void)]
             [else ((super type-check-exp env) e)])]
          
          [(Prim 'vectorof-length (list e1))
           (define-values (e1^ t1) (recur e1))
	   (debug 'type-check-exp "vectorof-length type: " t1)
           (match t1
             [`(Vectorof ,t)
              (values (Prim 'vectorof-length (list e1^))  'Integer)]
             [else
	      ;; error here instead? -Jeremy
	      ((super type-check-exp env) e)])]

          [(AllocateArray e1 t)
           (define-values (e1^ t1) (recur e1))
           (check-type-equal? t1 'Integer e)
           (values (AllocateArray e1^ t) t)]

          [(HasType e t)
           ((type-check-exp env) e)]
          
          [else ((super type-check-exp env) e)])))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Lvecof

(define type-check-Lvecof-class
  (class (type-check-vecof-mixin type-check-Lvec-class)
    (super-new)
    (inherit check-type-equal?)

    (define/override (operator-types)
      (append '((* . ((Integer Integer) . Integer))
                (exit . (() . _)))
              (super operator-types)))
    
    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "vecof" e)
        (define recur (type-check-exp env))
        (match e
          [(Prim 'vector-ref (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (match t1
             [`(Vectorof ,elt-type)
              (define-values (e2^ t2) (recur e2))
              (define e1^^ (cond [(typed-vecof) (HasType e1^ t1)]
                                 [else e1^]))
              (check-type-equal? t2 'Integer e2)
              (values (Prim 'vector-ref (list e1^^ e2^))
                      elt-type)]
             [else ((super type-check-exp env) e)])]
          [(Prim 'vector-set! (list e1 e2 e3) )
           (define-values (e-vec t-vec) (recur e1))
           (match t-vec
             [`(Vectorof ,elt-type)
              (define e-vec^ (cond [(typed-vecof) (HasType e-vec t-vec)]
                                   [else e-vec]))
              (define-values (e2^ t2) (recur e2))
              (define-values (e-arg^ t-arg) (recur e3))
              (check-type-equal? t2 'Integer e2)
              (check-type-equal? elt-type t-arg e)
              (values (Prim 'vector-set! (list e-vec^ e2^ e-arg^))  'Void)]
             [else ((super type-check-exp env) e)])]
          [(Prim 'vector-length (list e1))
           (define-values (e1^ t1) (recur e1))
           (define e1^^ (cond [(typed-vecof) (HasType e1^ t1)]
                              [else e1^]))
           (match t1
             [`(Vectorof ,t)
              (values (Prim 'vector-length (list e1^^))  'Integer)]
             [else ((super type-check-exp env) e)])]
          [else ((super type-check-exp env) e)])))

    ))

(define (type-check-Lvecof p)
  (send (new type-check-Lvecof-class) type-check-program p))

(define (type-check-Lvecof-has-type p)
  (typed-vecof #t)
  (typed-vec #t)
  (define result (send (new type-check-Lvecof-class) type-check-program p))
  (typed-vecof #f)
  (typed-vec #f)
  result)
