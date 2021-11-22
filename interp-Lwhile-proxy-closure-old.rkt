#lang racket
(require "utilities.rkt")
(require "interp-Lwhile-prime.rkt")
(provide interp-Lwhile-proxy-closure
         interp-Lwhile-proxy-closure-mixin
         interp-Lwhile-proxy-closure-class)

(define (interp-Lwhile-proxy-closure-mixin super-class)
  (class super-class
    (super-new)
    (inherit apply-fun interp-def)
    (inherit-field uninitialized)

    (define/public (apply-closure fun-val arg-vals e)
      (let ([f (vector-ref fun-val 0)])
        (apply-fun f (cons fun-val arg-vals) e)))
    
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
        ['inject-vector (lambda (v) v)]
        ['inject-proxy (lambda (v) `(vector-proxy ,v))]
        ['proxy? (match-lambda
                   [`(vector-proxy ,v) #t]
                   [else #f])]
        ['project-vector (lambda (v) v)]
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
        [else (super interp-op op)]))

    (define/override (interp-exp env)
      (lambda (ast)
	(match ast
          [(AllocateProxy ty)
           (define len 3)
           (build-vector len (lambda a uninitialized))]
	  [else ((super interp-exp env) ast)]
          )))
    
    ))

(define interp-Lwhile-proxy-closure-class
  (interp-Lwhile-proxy-closure-mixin interp-Lwhile-prime-class))

(define (interp-Lwhile-proxy-closure p)
  (send (new interp-Lwhile-proxy-closure-class) interp-program p))

