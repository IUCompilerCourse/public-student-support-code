#lang racket
;(require racket/fixnum)
(require "utilities.rkt")
(require "interp-Lany.rkt")
(provide interp-Lcast interp-Lcast-class)

(define interp-Lcast-class
  (class interp-Lany-class
    (super-new)
    (inherit apply-fun apply-inject apply-project)

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
      (verbose "Lcast/interp-op" op)
      (match op
        ['vector-length guarded-vector-length]
        ['vector-ref guarded-vector-ref]
        ['vector-set! guarded-vector-set!]
        ['any-vector-ref (lambda (v i)
                           (match v [(Tagged v^ tg)
                                     (guarded-vector-ref v^ i)]))]
        ['any-vector-set! (lambda (v i a)
                            (match v [(Tagged v^ tg)
                                      (guarded-vector-set! v^ i a)]))]
        ['any-vector-length (lambda (v)
                              (match v [(Tagged v^ tg)
                                        (guarded-vector-length v^)]))]
        [else (super interp-op op)]
        ))

    (define/public (apply-cast v s t)
      (match* (s t)
        [(t1 t2) #:when (equal? t1 t2) v]
        [('Any t2) 
         (match t2
           [`(,ts ... -> ,rt)
            (define any->any `(,@(for/list ([t ts]) 'Any) -> Any))
            (define v^ (apply-project v any->any))
            (apply-cast v^ any->any `(,@ts -> ,rt))]
           [`(Vector ,ts ...)
            (define vec-any `(Vector ,@(for/list ([t ts]) 'Any)))
            (define v^ (apply-project v vec-any))
            (apply-cast v^ vec-any `(Vector ,@ts))]
           [else (apply-project v t2)])]
        [(t1 'Any) 
         (match t1
           [`(,ts ... -> ,rt)
            (define any->any `(,@(for/list ([t ts]) 'Any) -> Any))
            (define v^ (apply-cast v `(,@ts -> ,rt) any->any))
            (apply-inject v^ (any-tag any->any))]
           [`(Vector ,ts ...)
            (define vec-any `(Vector ,@(for/list ([t ts]) 'Any)))
            (define v^ (apply-cast v `(Vector ,@ts) vec-any))
            (apply-inject v^ (any-tag vec-any))]
           [else (apply-inject v (any-tag t1))])]
        [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
         (define x (gensym 'x))
         (define cast-reads (for/list ([t1 ts1] [t2 ts2])
                              (Function (list x) (Cast (Var x) t1 t2) '())))
         (define cast-writes
           (for/list ([t1 ts1] [t2 ts2])
             (Function (list x) (Cast (Var x) t2 t1) '())))
         `(vector-proxy ,(vector v (apply vector cast-reads)
                                 (apply vector cast-writes)))]
        [(`(,ts1 ... -> ,rt1) `(,ts2 ... -> ,rt2))
         (define xs (for/list ([t2 ts2]) (gensym 'x)))
         (Function xs (Cast
                       (Apply (Value v)
                              (for/list ([x xs][t1 ts1][t2 ts2])
                                (Cast (Var x) t2 t1)))
                       rt1 rt2)
                   '())]
        ))
    
    (define/override ((interp-exp env) e)
      (define (recur e) ((interp-exp env) e))
      (verbose "Lcast/interp-exp" e)
      (define result
        (match e
          [(Value v) v]
          [(Cast e src tgt)
           (apply-cast (recur e) src tgt)]
          [else ((super interp-exp env) e)]))
      (verbose "Lcast/interp-exp" e result)
      result)
    
    ))

(define (interp-Lcast p)
  (send (new interp-Lcast-class) interp-program p))

