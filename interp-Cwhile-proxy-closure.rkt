#lang racket
(require "utilities.rkt")
(require "interp-Lwhile-proxy-closure.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(require "interp-Cvec.rkt")
(require "interp-Cfun.rkt")
(require "interp-Clambda.rkt")
(require "interp-Cwhile.rkt")
(provide interp-Cwhile-proxy-closure)

(define Cwhile-class (interp-Cwhile-mixin
                  (interp-Clambda-mixin
                   (interp-Cfun-mixin
                    (interp-Cvec-mixin
                     (interp-Cif-mixin
                      (interp-Cvar-mixin
                       interp-Lwhile-proxy-closure-class)))))))

(define (interp-Cwhile-proxy-closure p)
  (send (new Cwhile-class) interp-program p))
