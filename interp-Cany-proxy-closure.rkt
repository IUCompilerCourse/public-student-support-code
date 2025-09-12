#lang racket
(require "utilities.rkt")
(require "interp_Lany_proxy_closure.rkt")
(require "interp_Cvar.rkt")
(require "interp_Cif.rkt")
(require "interp_Cwhile.rkt")
(require "interp_Cvec.rkt")
(require "interp_Cvecof.rkt")
(require "interp_Cfun.rkt")
(require "interp_Clambda.rkt")
(provide interp_Cany_proxy_closure)

(define (interp_Cany_proxy_closure-mixin super-class)
  (class super-class
    (super-new)
    (inherit call-function)

    (define (apply-closure fun-val arg-vals e)
      (let ([f (vector-ref fun-val 0)])
        (call-function f (cons fun-val arg-vals) e)))
    
    (define (guarded-vector-ref vec i)
      (match vec
        [`(vector-proxy ,proxy)
         (define val (guarded-vector-ref (vector-ref proxy 0) i))
         (define rd (vector-ref (vector-ref proxy 1) i))
         (apply-closure rd (list val) 'guarded-vector-ref)]
        [else (vector-ref vec i)]))

    (define (guarded-vector-set! vec i arg)
      (match vec
        [`(vector-proxy ,proxy)
         (define wr (vector-ref (vector-ref proxy 2) i))
         (define arg^ (apply-closure wr (list arg) 'guarded-vector-set!))
         (guarded-vector-set! (vector-ref proxy 0) i arg^)]
        [else (vector-set! vec i arg)]))

    (define (guarded-vector-length vec)
      (match vec
        [`(vector-proxy ,proxy)
         (guarded-vector-length (vector-ref proxy 0))]
        [else (vector-length vec)]))
    
    (define/override (interp-op op)
      (match op
        ['proxy-vector-ref guarded-vector-ref]
        ['proxy-vector-set! guarded-vector-set!]
        ['proxy-vector-length guarded-vector-length]
        ['any-vector-ref (lambda (v i)
                           (match v [(Tagged v^ tg)
                                     (guarded-vector-ref v^ i)]))]
        ['any-vector-set! (lambda (v i a)
                            (match v [(Tagged v^ tg)
                                      (guarded-vector-set! v^ i a)]))]
        ['any-vector-length (lambda (v)
                              (match v [(Tagged v^ tg)
                                        (guarded-vector-length v^)]))]
        ['any-vectorof-ref (lambda (v i)
                           (match v [(Tagged v^ tg)
                                     (guarded-vector-ref v^ i)]))]
        ['any-vectorof-set! (lambda (v i a)
                            (match v [(Tagged v^ tg)
                                      (guarded-vector-set! v^ i a)]))]
        ['any-vectorof-length (lambda (v)
                              (match v [(Tagged v^ tg)
                                        (guarded-vector-length v^)]))]
        [else (super interp-op op)]))

    ))

(define Cany_proxy_closure-class
  (interp_Cany_proxy_closure-mixin
   (interp_Clambda-mixin
    (interp_Cfun-mixin
     (interp_Cvecof-mixin
      (interp_Cvec-mixin
       (interp_Cwhile-mixin
        (interp_Cif-mixin
         (interp_Cvar-mixin
          interp_Lany_proxy_closure-class)))))))))

(define (interp_Cany_proxy_closure p)
  (send (new Cany_proxy_closure-class) interp-program p))
