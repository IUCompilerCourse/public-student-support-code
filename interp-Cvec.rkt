#lang racket
(require "utilities.rkt")
(require "interp-Lvec-prime.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(require "interp-Cwhile.rkt")
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
          #;[(Prim 'vector-set! (list e-vec i e-arg))
           ((interp-exp env) ast)
           env]
          ;; TODO: move the following to the interpreter for any
          #;[(Prim 'any-vector-set! (list e-vec i e-arg))
           ((interp-exp env) ast)
           env]
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

    (define/override (interp-tail env blocks)
      (lambda (ast)
        (copious "interp-tail" ast)
        (match ast
          [(Seq s t)
           (define new-env ((interp-stmt env) s))
           ((interp-tail new-env blocks) t)]
          [else ((super interp-tail env blocks) ast)]
          )))
    
    (define/override (interp-program ast)
      (copious "interp-program" ast)
      (match ast
        [(CProgram info blocks)
         ((initialize!) runtime-config:rootstack-size
                        runtime-config:heap-size)
         (super interp-program (CProgram info blocks))]
        [else (error "interp-program unhandled" ast)]))
    ))

(define (interp-Cvec p)
  (define Cvec-class (interp-Cvec-mixin
                      (interp-Cwhile-mixin
                       (interp-Cif-mixin
                        (interp-Cvar-mixin
                         interp-Lvec-prime-class)))))
  (send (new Cvec-class) interp-program p))

