#lang racket
(require "interp-Lwhile-proxy-closure.rkt")
(require "interp-Lvecof-prime.rkt")
(provide interp-Lvecof-proxy-closure interp-Lvecof-proxy-closure-class)


(define interp-Lvecof-proxy-closure-class
  (interp-Lwhile-proxy-closure-mixin interp-Lvecof-prime-class))

(define (interp-Lvecof-proxy-closure p)
  (send (new interp-Lvecof-proxy-closure-class) interp-program p))
