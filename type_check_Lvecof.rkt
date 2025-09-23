#lang racket
(require "utilities.rkt")
(require "type_check_Lvec.rkt")
(provide type_check_Lvecof type_check_Lvecof_has_type
         type_check_Lvecof-class type_check_vecof-mixin typed-vecof)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Homogeneous Vectors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Lvecof

(define typed-vecof (make-parameter #f))

(define (type_check_vecof-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal?)

    (define/override (operator-types)
      (append '((* . ((Integer Integer) . Integer))
                (exit . (() . _)))
              (super operator-types)))

    (define/override (type_check_exp env)
      (lambda (e)
        (debug 'type_check_exp "vecof" e)
        (define recur (type_check_exp env))
        (match e
          [(Prim 'make-vector (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ elt-type) (recur e2))
           (define vec-type `(Vectorof ,elt-type))
           (define mk-vec (Prim 'make-vector (list e1^ e2^)))
           (values (cond [(typed-vecof) (HasType mk-vec vec-type)]
                         [else mk-vec])
                   vec-type)]
          [(Prim 'vectorof-ref (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (check-type-equal? t2 'Integer e2)
           (match t1
             [`(Vectorof ,elt-type)
              (values (Prim 'vectorof-ref (list e1^ e2^)) elt-type)]
             [else
              (error 'type_check
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
             [else ((super type_check_exp env) e)])]
          [(Prim 'vectorof-length (list e1))
           (define-values (e1^ t1) (recur e1))
	   (debug 'type_check_exp "vectorof-length type: " t1)
           (match t1
             [`(Vectorof ,t)
              (values (Prim 'vectorof-length (list e1^))  'Integer)]
             [else
	      ;; error here instead? -Jeremy
	      ((super type_check_exp env) e)])]

          [(AllocateArray e1 t)
           (define-values (e1^ t1) (recur e1))
           (check-type-equal? t1 'Integer e)
           (values (AllocateArray e1^ t) t)]

          [(HasType e t)
           ((type_check_exp env) e)]
          
          [else ((super type_check_exp env) e)])))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Lvecof

(define type_check_Lvecof-class
  (class (type_check_vecof-mixin type_check_Lvec-class)
    (super-new)
    (inherit check-type-equal?)

    (define/override (operator-types)
      (append '((* . ((Integer Integer) . Integer))
                (exit . (() . _)))
              (super operator-types)))
    
    (define/override (type_check_exp env)
      (lambda (e)
        (debug 'type_check_exp "vecof" e)
        (define recur (type_check_exp env))
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
             [else ((super type_check_exp env) e)])]
          [(Prim 'vector-set! (list e1 e2 e3) )
           (define-values (e-vec t-vec) (recur e1))
           (match t-vec
             [`(Vectorof ,elt-type)
              (define-values (e2^ t2) (recur e2))
              (define-values (e-arg^ t-arg) (recur e3))
              (check-type-equal? t2 'Integer e2)
              (check-type-equal? elt-type t-arg e)
              (values (Prim 'vectorof-set! (list e-vec e2^ e-arg^))  'Void)]
             [else ((super type_check_exp env) e)])]
          [(Prim 'vector-length (list e1))
           (define-values (e1^ t1) (recur e1))
           (match t1
             [`(Vectorof ,t)
              (values (Prim 'vectorof-length (list e1^))  'Integer)]
             [else ((super type_check_exp env) e)])]
          [else ((super type_check_exp env) e)])))

    ))

(define (type_check_Lvecof p)
  (send (new type_check_Lvecof-class) type_check_program p))

(define (type_check_Lvecof_has_type p)
  (typed-vecof #t)
  (typed-vec #t)
  (define result (send (new type_check_Lvecof-class) type_check_program p))
  (typed-vecof #f)
  (typed-vec #f)
  result)
