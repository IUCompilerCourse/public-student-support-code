#lang racket
(require "interp_Lwhile_proxy_closure.rkt")
(require "interp_Lvecof_prime.rkt")
(provide interp_Lvecof_proxy_closure interp_Lvecof_proxy_closure-class)


(define interp_Lvecof_proxy_closure-class
  (interp_Lwhile_proxy_closure-mixin interp_Lvecof_prime-class))

(define (interp_Lvecof_proxy_closure p)
  (send (new interp_Lvecof_proxy_closure-class) interp-program p))
