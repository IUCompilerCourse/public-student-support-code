#lang racket
(require "utilities.rkt")
(require "interp_Lwhile_proxy_closure.rkt")
(require "interp_Cvar.rkt")
(require "interp_Cif.rkt")
(require "interp_Cvec.rkt")
(require "interp_Cfun.rkt")
(require "interp_Clambda.rkt")
(require "interp_Cwhile.rkt")
(provide interp_Cwhile_proxy_closure)

(define Cwhile-class (interp_Cwhile-mixin
                  (interp_Clambda-mixin
                   (interp_Cfun-mixin
                    (interp_Cvec-mixin
                     (interp_Cif-mixin
                      (interp_Cvar-mixin
                       interp_Lwhile_proxy_closure-class)))))))

(define (interp_Cwhile_proxy_closure p)
  (send (new Cwhile-class) interp-program p))
