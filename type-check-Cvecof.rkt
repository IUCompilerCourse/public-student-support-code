#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Cif.rkt")
(require "type-check-Cvec.rkt")
(require "type-check-Cfun.rkt")
(require "type-check-Cany.rkt")
(require "type-check-Cwhile.rkt")
(require "type-check-Lvecof.rkt")
(provide type-check-Cvecof type-check-Cvecof-mixin type-check-Cvecof-class)

(define (type-check-Cvecof-mixin super-class)
  (class super-class
    (super-new)

    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(AllocateHom e-len ty) (recur e-len)]
	[else (super free-vars-exp e)]))

    ))

(define type-check-Cvecof-class
  (type-check-Cvecof-mixin
   (type-check-Cwhile-mixin
    (type-check-Cany-mixin
     (type-check-Cfun-mixin
      (type-check-Cvec-mixin
       (type-check-Cif-mixin
        (type-check-Cvar-mixin
         type-check-Lvecof-class))))))))

(define (type-check-Cvecof p)
  (send (new type-check-Cvecof-class) type-check-program p))

        
