#lang racket
(require "interp-Rvec-prime.rkt")
(require "interp-Rfun.rkt")
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Rfun-prime interp-Rfun-prime-mixin interp-Rfun-prime-class)

(define (interp-Rfun-prime-mixin super-class)
  (class super-class
    (super-new)
    (inherit initialize! interp-def)

    (define/override ((interp-exp env) e)
      (verbose "Rfun'/interp-exp" e)
      (match e
        [(FunRef f)
         (unbox (lookup f env))]
        [(FunRefArity f n)
         (unbox (lookup f env))]
        [else ((super interp-exp env) e)]
        ))

    (define/override (interp-program ast)
      (verbose "interp-program" ast)
      (match ast
        [(ProgramDefs info ds)
         ((initialize!) runtime-config:rootstack-size
                        runtime-config:heap-size)
         (define top-level (for/list ([d ds]) (interp-def d)))
         (for ([f (in-dict-values top-level)])
           (set-box! f (match (unbox f)
                         [`(function ,xs ,body ())
                          `(function ,xs ,body ,top-level)])))
         ((interp-exp top-level) (Apply (Var 'main) '()))]))
        
    ))

(define interp-Rfun-prime-class (interp-Rfun-prime-mixin
                               (interp-Rvec-prime-mixin interp-Rfun-class)))
    
(define (interp-Rfun-prime p)
  (send (new interp-Rfun-prime-class) interp-program p))
