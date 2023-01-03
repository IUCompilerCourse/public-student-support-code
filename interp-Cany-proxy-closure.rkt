#lang racket
(require "utilities.rkt")
(require "interp-Lany-proxy-closure.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(require "interp-Cwhile.rkt")
(require "interp-Cvec.rkt")
(require "interp-Cvecof.rkt")
(require "interp-Cfun.rkt")
(require "interp-Clambda.rkt")
(provide interp-Cany-proxy-closure)

(define (interp-Cany-proxy-closure-mixin super-class)
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

(define Cany-proxy-closure-class
  (interp-Cany-proxy-closure-mixin
   (interp-Clambda-mixin
    (interp-Cfun-mixin
     (interp-Cvecof-mixin
      (interp-Cvec-mixin
       (interp-Cwhile-mixin
        (interp-Cif-mixin
         (interp-Cvar-mixin
          interp-Lany-proxy-closure-class)))))))))

(define (interp-Cany-proxy-closure p)
  (send (new Cany-proxy-closure-class) interp-program p))
