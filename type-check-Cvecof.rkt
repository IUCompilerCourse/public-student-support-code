#lang racket
(require "utilities.rkt")
(require "type-check-Cvec.rkt")
(require "type-check-Lvecof.rkt")
(provide type-check-Cvecof type-check-Cvecof-class)

(define type-check-Cvecof-class
  (class (type-check-vecof-mixin type-check-Cvec-class)
    (super-new)
    (inherit check-type-equal?)

    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(AllocateArray e-len ty) (recur e-len)]
	[else (super free-vars-exp e)]))

    (define/override ((type-check-tail env block-env blocks) t)
      (debug 'type-check-tail "Cif" t)
      (match t
        [(Prim 'exit '())
         '_]
        [else ((super type-check-tail env block-env blocks) t)]))
    
    ))

(define (type-check-Cvecof p)
  (send (new type-check-Cvecof-class) type-check-program p))

        
