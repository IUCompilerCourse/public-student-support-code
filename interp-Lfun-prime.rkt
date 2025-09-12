#lang racket
(require "interp_Lvecof_prime.rkt")
(require "interp_Lvec_prime.rkt")
(require "interp_Lfun.rkt")
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp_Lfun_prime interp_Lfun_prime-mixin interp_Lfun_prime-class)

(define (interp_Lfun_prime-mixin super-class)
  (class super-class
    (super-new)
    (inherit initialize! interp-def)

    (define/override ((interp_exp env) e)
      (verbose "Lfun'/interp_exp" e)
      (match e
        [(FunRef f n)
         (unbox (lookup f env))]
        [else ((super interp_exp env) e)]
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
         ((interp_exp top-level) (Apply (Var 'main) '()))]))
        
    ))

(define interp_Lfun_prime-class
  (interp_Lfun_prime-mixin
   (interp_Lvecof_prime-mixin
    (interp_Lvec_prime-mixin
     interp_Lfun-class))))
    
(define (interp_Lfun_prime p)
  (send (new interp_Lfun_prime-class) interp-program p))
