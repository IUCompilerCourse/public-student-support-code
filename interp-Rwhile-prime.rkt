#lang racket
(require "interp-Rvec-prime.rkt")
(require "interp-Rfun-prime.rkt")
(require "interp-Rlambda-prime.rkt")
(require "interp-Rany-prime.rkt")
(require "interp-Rwhile.rkt")
(require "utilities.rkt")
(provide interp-Rwhile-prime interp-Rwhile-prime-class)

(define interp-Rwhile-prime-class
  (interp-Rany-prime-mixin
   (interp-Rlambda-prime-mixin
    (interp-Rfun-prime-mixin
     (interp-Rvec-prime-mixin interp-Rwhile-class)))))
    
(define (interp-Rwhile-prime p)
  (send (new interp-Rwhile-prime-class) interp-program p))
