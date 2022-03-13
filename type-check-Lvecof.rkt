#lang racket
(require "utilities.rkt")
(require "type-check-Lvec.rkt")
(provide type-check-Lvecof type-check-Lvecof-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Homogeneous Vectors                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Lvecof

(define type-check-Lvecof-class
  (class type-check-Lvec-class
    (super-new)
    (inherit check-type-equal?)

    (define/override (operator-types)
      (append '((* . ((Integer Integer) . Integer)))
              (super operator-types)))
    
    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "Lvecof" e)
        (define recur (type-check-exp env))
        (match e
          [(Prim 'make-vector (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ elt-type) (recur e2))
           (define vec-type `(Vectorof ,elt-type))
           (values (HasType (Prim 'make-vector (list e1^ e2^)) vec-type)
                   vec-type)]
          [(HasType (Prim 'make-vector es) t)
           (recur (Prim 'make-vector es))]

          [(Prim (or 'vector-ref 'vectorof-ref) (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (match* (t1 t2)
             [(`(Vectorof ,elt-type) 'Integer)
              (values (Prim 'vectorof-ref (list e1^ e2^)) elt-type)]
             [(other wise) ((super type-check-exp env) e)])]
          [(Prim (or 'vector-set! 'vectorof-set!) (list e1 e2 e3) )
           (define-values (e-vec t-vec) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (define-values (e-arg^ t-arg) (recur e3))
           (match t-vec
             [`(Vectorof ,elt-type)
              (check-type-equal? elt-type t-arg e)
              (values (Prim 'vectorof-set! (list e-vec e2^ e-arg^))  'Void)]
             [else ((super type-check-exp env) e)])]
          [(Prim (or 'vector-length 'vectorof-length) (list e1))
           (define-values (e1^ t1) (recur e1))
           (match t1
             [`(Vectorof ,t)
              (values (Prim 'vectorof-length (list e1^))  'Integer)]
             [else ((super type-check-exp env) e)])]

          [(AllocateHom e1 t)
           (define-values (e1^ t1) (recur e1))
           (check-type-equal? t1 'Integer e)
           (values (AllocateHom e1^ t) t)]
          
          [else ((super type-check-exp env) e)])))
    ))

(define (type-check-Lvecof p)
  (send (new type-check-Lvecof-class) type-check-program p))


