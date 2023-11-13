#lang racket
(require "utilities.rkt")
(require "interp-Lfun-prime.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(require "interp-Cwhile.rkt")
(require "interp-Cvec.rkt")
(require "interp-Cvecof.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Cfun interp-Cfun-mixin)

(define (interp-Cfun-mixin super-class)
  (class super-class
    (super-new)
    (inherit initialize!)

    (define/override (interp-stmt env)
      (lambda (s)
        (match s
          [(Call e es)
           (define f-val ((interp-exp env) e))
           (define arg-vals (map (interp-exp env) es))
           (call-function f-val arg-vals s)
           env]
          #;[(Assign (Var x) e)
           (dict-set env x (box ((interp-exp env) e)))]
          [else ((super interp-stmt env) s)]
          )))
    
    (define/public (call-function fun arg-vals ast)
      (match fun
        [(CFunction xs info blocks def-env)
         (define f (dict-ref info 'name))
         (define f-start (symbol-append f '_start))
         (define params-args (for/list ([x xs] [arg arg-vals])
                               (cons x (box arg))))
         (define new-env (append params-args def-env))
         ((interp-tail new-env blocks) (dict-ref blocks f-start))]
        [else (error 'interp-exp "expected C function, not ~a\nin ~v" fun ast)]))
    
    (define/override ((interp-exp env) ast)
      (define result
        (match ast
          [(Call f args)
           (define f-val ((interp-exp env) f))
           (define arg-vals (map (interp-exp env) args))
           (call-function f-val arg-vals ast)]
          [else ((super interp-exp env) ast)]))
      (verbose 'interp-exp ast result)
      result)

    (define/override ((interp-tail env blocks) ast)
      (match ast
        [(TailCall f args)
         (define arg-vals (map (interp-exp env) args))
         (define f-val ((interp-exp env) f))
         (call-function f-val arg-vals ast)]
        [else ((super interp-tail env blocks) ast)]))

    (define/override (interp-def ast)
      (match ast
        [(Def f `([,xs : ,ps] ...) rt info blocks)
         (cons f (box (CFunction xs `((name . ,f)) blocks '())))]
        [else (error 'interp-def "unhandled" ast)]
        ))

    (define/override (interp-program ast)
      (match ast
        [(ProgramDefs info ds)
         ((initialize!) runtime-config:rootstack-size
                        runtime-config:heap-size)
         (define top-level (for/list ([d ds]) (interp-def d)))
         (for/list ([f (in-dict-values top-level)])
           (set-box! f (match (unbox f)
                          [(CFunction xs info blocks '())
                           (CFunction xs info blocks top-level)])))
         ((interp-tail top-level '()) (TailCall (Var 'main) '()))]
        [else (error 'interp-program "unhandled ~a" ast)]
        ))
    
    ))

(define (interp-Cfun p)
  (define Cfun-class (interp-Cfun-mixin
                      (interp-Cvecof-mixin
                       (interp-Cvec-mixin
                        (interp-Cwhile-mixin
                         (interp-Cif-mixin
                          (interp-Cvar-mixin
                           interp-Lfun-prime-class)))))))
  (send (new Cfun-class) interp-program p))
