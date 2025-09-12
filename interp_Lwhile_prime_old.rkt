#lang racket
(require "interp_Lvec_prime.rkt")
(require "interp_Lfun_prime.rkt")
(require "interp_Llambda_prime.rkt")
(require "interp_Lany_prime.rkt")
(require "interp_Lwhile.rkt")
(require "utilities.rkt")
(provide interp_Lwhile_prime interp_Lwhile_prime-class)

(define interp_Lwhile_prime-class
  (interp_Lany_prime-mixin
   (interp_Llambda_prime-mixin
    (interp_Lfun_prime-mixin
     (interp_Lvec_prime-mixin interp_Lwhile-class)))))
    
(define (interp_Lwhile_prime p)
  (send (new interp_Lwhile_prime-class) interp-program p))
