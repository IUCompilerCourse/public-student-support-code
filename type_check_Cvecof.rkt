#lang racket
(require "utilities.rkt")
(require "type_check_Cvec.rkt")
(require "type_check_Lvecof.rkt")
(provide type_check_Cvecof type_check_Cvecof-class)

(define type_check_Cvecof-class
  (class (type_check_vecof-mixin type_check_Cvec-class)
    (super-new)
    (inherit check-type-equal?)

    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(AllocateArray e-len ty) (recur e-len)]
	[else (super free-vars-exp e)]))

    (define/override ((type_check_tail env block-env blocks) t)
      (debug 'type_check_tail "Cif" t)
      (match t
        [(Prim 'exit '())
         '_]
        [else ((super type_check_tail env block-env blocks) t)]))
    
    ))

(define (type_check_Cvecof p)
  (send (new type_check_Cvecof-class) type_check_program p))

        
