#lang racket
(require "utilities.rkt")


(provide make-cast apply-inject apply-project flat-type-eq?)

(define (make-cast e s t)
  (verbose "make-cast " s t)
  (cond [(equal? s t)
         ;; This is obervable because eq? doesn't dive through proxies,
         ;; but should it? -Jeremy
         e]
        [else
         (match (list s t)
           [`(Integer Integer) e]
           [`(Boolean Boolean) e]
           [`(Void Void) e]
           [`(Nothing Nothing) e]
           [`(Any Any) e]
           [`(Any ,t2) 
            (match t2
              [`(,ts ... -> ,rt)
               (define any->any `(,@(for/list ([t ts]) 'Any) -> Any))
               ;; first project from (,ts ... -> ,rt) to (Any ... -> Any)
               (define e^ (HasType (Project e any->any) any->any))
               ;; then cast from (Any ... -> Any) to (,ts ... -> ,rt)
               (make-cast e^ any->any `(,@ts -> ,rt))]
              [`(Vector ,ts ...)
               (define vec-any `(Vector ,@(for/list ([t ts]) 'Any)))
               ;; first project to (Vector Any ...)
               (define e^ (HasType (Project e vec-any) vec-any))
               ;; then cast from (Vector Any ...) to (Vector ,ts ...)
               (make-cast e^ vec-any `(Vector ,@ts))]
              [`(Vectorof ,t)
               (define vec-any `(Vectorof Any))
               ;; first project to (Vectorof Any)
               (define e^ (HasType (Project e vec-any) vec-any))
               ;; then cast from (Vectorof Any) to (Vectorof ,t)
               (make-cast e^ vec-any `(Vectorof ,t))]
              [else
               (HasType (Project e t2) t2)])]
           [`(,t1 Any) 
            (match t1
              [`(,ts ... -> ,rt)
               (define any->any `(,@(for/list ([t ts]) 'Any) -> Any))
               ;; first cast from (,ts ... -> ,rt) to (Any ... -> Any) 
               (define e^ (make-cast e `(,@ts -> ,rt) any->any))
               ;; then inject from (Any ... -> Any) to ANy
               (HasType (Inject e^ any->any) 'Any)]
              [`(Vector ,ts ...)
               (define vec-any `(Vector ,@(for/list ([t ts]) 'Any)))
               ;; first cast (Vector ,ts ...) from (Vector Any ...) 
               (define e^ (make-cast e `(Vector ,@ts) vec-any))
               ;; then inject from (Vector Any ...) to Any
               (HasType (Inject e^ vec-any) 'Any)]
              [`(Vectorof ,t)
               (define vec-any `(Vectorof Any))
               ;; first cast from (Vectorof ,t) to (Vectorof Any)
               (define e^ (make-cast e `(Vectorof ,t) vec-any))
               ;; then inject from (Vectorof Any) to Any
               (HasType (Inject e^ vec-any) 'Any)]
              [else
               (HasType (Inject e t1) 'Any)])]
           [`((Vectorof ,t1) (Vectorof ,t2))
            (let ([x (gensym 'x)])
              (HasType (Prim
                        'vectorof-proxy
                        (list e
                              ;; reading
                              (HasType
                               (Lambda `([,x : ,t1]) t2 
                                       (make-cast (HasType (Var x) t1) t1 t2))
                               `(,t1 -> ,t2))
                              ;; writing
                              (HasType
                               (Lambda `([,x : ,t2]) t1 
                                       (make-cast (HasType (Var x) t2) t2 t1))
                               `(,t2 -> ,t1))
                              ))
                       `(Vectorof ,t2)))]
           [`((Vector ,ts1 ...) (Vector ,ts2 ...))
            (let ([x (gensym 'x)])
              (HasType 
                (Prim 'vector-proxy
                      (list e
                            ;; reading
                            (HasType
                             (Prim 'vector
                                   (for/list ([t1 ts1] [t2 ts2])
                                     (HasType 
                                         (Lambda `([,x : ,t1]) t2
                                                 (make-cast (HasType (Var x) t1)
                                                            t1 t2))
                                         `(,t1 -> ,t2))))
                             `(Vector _))
                            ;; writing
                            (HasType
                             (Prim 'vector
                                   (for/list ([t1 ts1] [t2 ts2])
                                     (HasType
                                      (Lambda `([,x : ,t2]) t1 
                                              (make-cast (HasType (Var x) t2)
                                                         t2 t1))
                                      `(,t2 -> ,t1))))
                             `(Vector _))
                            ))
                `(Vector ,@ts2)))]
           [`((,ts1 ... -> ,rt1) (,ts2 ... -> ,rt2))
            (define xs (for/list ([t2 ts2]) (gensym 'x)))
            (define params (for/list ([x xs] [t2 ts2]) `[,x : ,t2]))
            (HasType (Lambda params rt2
                             (make-cast 
                              (Apply e (for/list ([x xs] 
                                                  [t1 ts1] [t2 ts2])
                                         (make-cast
                                          (HasType (Var x) t2) t2 t1)))
                              rt1 rt2))
                     `(,@ts2 -> ,rt2))]
           [else
            #f]
           )]))

(define (apply-inject v tg)
  `(tagged ,v ,tg))

;; Equality for flat types.
(define (flat-type-eq? t1 t2)
  (match `(,t1 ,t2)
    [`((Vectorof Any) (Vector ,t2s ...))
     (for/and ([t2 t2s])
       (eq? t2 'Any))]
    [`((Vector ,t1s ...) (Vectorof Any))
     (for/and ([t1 t1s])
       (eq? t1 'Any))]
    [else (equal? t1 t2)]))

(define (apply-project v ty2)
  (define tag2 (any-tag ty2))
  (match v
    [`(tagged ,v1 ,tag1)
     (cond [(eq? tag1 tag2)
            (match ty2
              [`(Vector ,ts ...)
               (cond [(eq? (vector-length v1) (length ts))
                      v1]
                     [else
                      (error 'apply-project
                             "vector length ~a does not match length of target type ~a"
                             (vector-length v1) (length ts))])]
              [`(,ts ... -> ,rt)
               (match v1
                 [`(function ,xs ,body ,env)
                  (cond [(eq? (length xs) (length ts))
                         v1]
                        [else
                         (error 'apply-project
                                "function arity ~a does not match arity of target type ~a"
                                (length xs) (length ts))])]
                 [else (error 'apply-project "expected a function, not ~a" v1)])]
              [else
               v1])]
           [else
            (error 'apply-project "tag mismatch ~a != ~a" tag1 tag2)])]
    [else
     (error 'apply-project "expected tagged value, not ~a" v)]))

#;(define (apply-cast v s t)
  (match (list s t)
    [`(Integer Integer) v]
    [`(Boolean Boolean) v]
    [`(Void Void) v]
    [`(Nothing Nothing) v]
    [`(Any Any) v]
    [`(Any ,t2) 
     (match t2
       [`(,ts ... -> ,rt)
        (define any->any `(,@(for/list ([t ts]) 'Any) -> Any))
        ;; first project from (,ts ... -> ,rt) to (Any ... -> Any)
        (define v^ (apply-project v any->any))
        ;; then cast from (Any ... -> Any) to (,ts ... -> ,rt)
        (apply-cast e^ any->any `(,@ts -> ,rt))]
       [`(Vector ,ts ...)
        (define vec-any `(Vector ,@(for/list ([t ts]) 'Any)))
        ;; first project to (Vector Any ...)
        (define v^ (apply-project v vec-any))
        ;; then cast from (Vector Any ...) to (Vector ,ts ...)
        (apply-cast v^ vec-any `(Vector ,@ts))]
       [`(Vectorof ,t)
        (define vec-any `(Vectorof Any))
        ;; first project to (Vectorof Any)
        (define v^ (apply-project v vec-any))
        ;; then cast from (Vectorof Any) to (Vectorof ,t)
        (apply-cast v^ vec-any `(Vectorof ,t))]
       [else
        (apply-project v t2)])]
    [`(,t1 Any) 
     (match t1
       [`(,ts ... -> ,rt)
        (define any->any `(,@(for/list ([t ts]) 'Any) -> Any))
        ;; first cast from (,ts ... -> ,rt) to (Any ... -> Any) 
        (define v^ (apply-cast v `(,@ts -> ,rt) any->any))
        ;; then inject from (Any ... -> Any) to ANy
        (apply-inject v^ any->any)]
       [`(Vector ,ts ...)
        (define vec-any `(Vector ,@(for/list ([t ts]) 'Any)))
        ;; first cast (Vector ,ts ...) from (Vector Any ...) 
        (define v^ (apply-cast v `(Vector ,@ts) vec-any))
        ;; then inject from (Vector Any ...) to Any
        (apply-inject v^ vec-any)]
       [`(Vectorof ,t)
        (define vec-any `(Vectorof Any))
        ;; first cast from (Vectorof ,t) to (Vectorof Any)
        (define v^ (apply-cast v `(Vectorof ,t) vec-any))
        ;; then inject from (Vectorof Any) to Any
        (apply-inject v^ vec-any)]
       [else
        (apply-inject v t1)])]
    [`((Vector ,ts1 ...) (Vector ,ts2 ...))
     (let ([x (gensym 'x)])
       `(vector-proxy
         ,v
         ;; reading
         ,@(for/list ([t1 ts1] [t2 ts2])
             `(function: ([,x : ,t1]) : ,t2 () ,(make-cast x t1 t2)))
         ;; writing
         ,@(for/list ([t1 ts1] [t2 ts2])
             `(function: ([,x : ,t2]) : ,t1 ,(make-cast x t2 t1)))
         ))]
    [`((,ts1 ... -> ,rt1) (,ts2 ... -> ,rt2))
     (define xs (for/list ([t2 ts2]) (gensym 'x)))
     (define params (for/list ([x xs] [t2 ts2]) `[,x : ,t2]))
     `(has-type (lambda: ,params : ,rt2
                         ,(apply-cast 
                           `(,e ,@(for/list ([x xs] [t1 ts1] [t2 ts2])
                                    (apply-cast x t2 t1)))
                           rt1 rt2))
                (,@ts2 -> ,rt2))]
    [`((Vectorof ,t1) (Vectorof ,t2))
     (let ([x (gensym 'x)])
       `(vectorof-proxy
         ,e
         ;; reading
         (has-type (lambda: ([,x : ,t1]) : ,t2 ,(apply-cast x t1 t2))
                   (,t1 -> ,t2))
         ;; writing
         (has-type (lambda: ([,x : ,t2]) : ,t1 ,(apply-cast x t2 t1))
                   (,t2 -> ,t1))
         ))]
    [else
     #f]
    ))
