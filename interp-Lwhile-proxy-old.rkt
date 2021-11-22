#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp-Lwhile-prime.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Lwhile-proxy interp-Lwhile-proxy-class)

(define interp-Lwhile-proxy-class
  (class interp-Lwhile-prime-class
    (super-new)
    (inherit apply-fun initialize! interp-def interp-exp)

    (define (guarded-vector-ref vec i)
      (match vec
        [`(vector-proxy ,proxy)
         (define val (guarded-vector-ref (vector-ref proxy 0) i))
         (define rd (vector-ref (vector-ref proxy 1) i))
         (apply-fun rd (list val) 'guarded-vector-ref)]
        [else (vector-ref vec i)]))

    (define (guarded-vector-set! vec i arg)
      (match vec
        [`(vector-proxy ,proxy)
         (define wr (vector-ref (vector-ref proxy 2) i))
         (define arg^ (apply-fun wr (list arg) 'guarded-vector-set!))
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

    (define/override (apply-project v ty2)
      (define tag2 (any-tag ty2))
      (match v
        [(Tagged v1 tag1)
         (cond [(eq? tag1 tag2)
                (match ty2
                  [`(PVector ,ts ...)
                   (define len ((interp-op 'proxy-vector-length) v1))
                   (cond [(eq? len (length ts)) v1]
                         [else
                          (error 'apply-project
                                 "incorrect vector length, ~a != ~a"
                                 len (length ts))])]
                  [else (super apply-project v ty2)])]
               [else (error 'apply-project "tag mismatch ~a != ~a" tag1 tag2)])]
        [else (error 'apply-project "expected tagged value, not ~a" v)]))
    
    (define/override (interp-program ast)
      (match ast
        ;; Before shrink
        [(ProgramDefsExp info ds body)
         ((initialize!) runtime-config:rootstack-size
                        runtime-config:heap-size)
         (define top-level (for/list ([d ds]) (interp-def d)))
         (for ([f (in-dict-values top-level)])
           (set-box! f (match (unbox f)
                         [`(function ,xs ,body ())
                          `(function ,xs ,body ,top-level)])))
         ((interp-exp top-level) body)]
        [else (super interp-program ast)]))
    
    ))

(define (interp-Lwhile-proxy p)
  (send (new interp-Lwhile-proxy-class) interp-program p))

