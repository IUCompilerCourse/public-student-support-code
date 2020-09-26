#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp-R2.rkt")
(provide interp-C1)

(define (interp-C1-stmt env)
  (lambda (s)
    (match s
      [(Assign (Var x) e)
       (cons (cons x ((interp-exp env) e)) env)]
      [else
       (error "interp-C1-stmt unmatched" s)]
      )))

(define (interp-C1-tail env CFG)
  (lambda (t)
    (match t
      [(Return e)
       ((interp-exp env) e)]
      [(Goto l)
       ((interp-C1-tail env CFG) (dict-ref CFG l))]
      [(IfStmt (Prim op arg*) (Goto thn-label) (Goto els-label))
       (if ((interp-exp env) (Prim op arg*))
           ((interp-C1-tail env CFG) (dict-ref CFG thn-label))
           ((interp-C1-tail env CFG) (dict-ref CFG els-label)))]
      [(Seq s t2)
       (define new-env ((interp-C1-stmt env) s))
       ((interp-C1-tail new-env CFG) t2)]
      [else
       (error "interp-C1-tail unmatched" t)]
      )))
  
(define (interp-C1 p)
  (match p
    [(Program info (CFG G))
     ((interp-C1-tail '() G) (dict-ref G 'start))]
    ))
