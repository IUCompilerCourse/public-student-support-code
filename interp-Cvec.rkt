#lang racket
(require "utilities.rkt")
(require "interp-Rvec-prime.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Cvec interp-Cvec-mixin)

(define (interp-Cvec-mixin super-class)
  (class super-class
    (super-new)
    (inherit interp-exp initialize!)

    (define/override (interp-stmt env)
      (lambda (ast)
        (copious "interp-stmt" ast)
        (match ast
	  ;; Determine if a collection is needed.
	  ;; Which it isn't because vectors stored in the environment
	  ;; is the representation of the heap in the C language,
	  ;; but collection is a no-op so we should check to see if
	  ;; everything is well formed anyhow.
	  ;; Collection isn't needed or possible in this representation
	  [(Collect size)
	   (unless (exact-nonnegative-integer? size)
	     (error 'interp-C "invalid argument to collect in ~a" ast))
	   env]
          [else ((super interp-stmt env) ast)]
          )))

    (define/override (interp-tail env CFG)
      (lambda (ast)
        (copious "interp-tail" ast)
        (match ast
          [(Seq s t)
           (define new-env ((interp-stmt env) s))
           ((interp-tail new-env CFG) t)]
          [else ((super interp-tail env CFG) ast)]
          )))
    
    (define/override (interp-program ast)
      (copious "interp-program" ast)
      (match ast
        [(CProgram info G)
         ((initialize!) runtime-config:rootstack-size
                        runtime-config:heap-size)
         (super interp-program (CProgram info G))]
        [else (error "interp-program unhandled" ast)]))
    ))

(define (interp-Cvec p)
  (define Cvec-class (interp-Cvec-mixin
                      (interp-Cif-mixin
                       (interp-Cvar-mixin
                        interp-Rvec-prime-class))))
  (send (new Cvec-class) interp-program p))

