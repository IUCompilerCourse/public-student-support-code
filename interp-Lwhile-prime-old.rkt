#lang racket
(require "interp-Lvec-prime.rkt")
(require "interp-Lfun-prime.rkt")
(require "interp-Llambda-prime.rkt")
(require "interp-Lany-prime.rkt")
(require "interp-Lwhile.rkt")
(require "utilities.rkt")
(provide interp-Lwhile-prime interp-Lwhile-prime-class)

(define interp-Lwhile-prime-class
  (interp-Lany-prime-mixin
   (interp-Llambda-prime-mixin
    (interp-Lfun-prime-mixin
     (interp-Lvec-prime-mixin interp-Lwhile-class)))))
    
(define (interp-Lwhile-prime p)
  (send (new interp-Lwhile-prime-class) interp-program p))
