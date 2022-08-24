#lang racket
(require "interp-Lvecof-prime.rkt")
(require "interp-Lvec-prime.rkt")
(require "interp-Lfun.rkt")
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Lfun-prime interp-Lfun-prime-mixin interp-Lfun-prime-class)

(define (interp-Lfun-prime-mixin super-class)
  (class super-class
    (super-new)
    (inherit initialize! interp-def)

    (define/override ((interp-exp env) e)
      (verbose "Lfun'/interp-exp" e)
      (match e
        [(FunRef f n)
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
                         [(Function xs body '())
                          (Function xs body top-level)])))
         ((interp-exp top-level) (Apply (Var 'main) '()))]))
        
    ))

(define interp-Lfun-prime-class
  (interp-Lfun-prime-mixin
   (interp-Lvecof-prime-mixin
    (interp-Lvec-prime-mixin
     interp-Lfun-class))))
    
(define (interp-Lfun-prime p)
  (send (new interp-Lfun-prime-class) interp-program p))
