#lang racket
(require "utilities.rkt")
(provide type-check-Lvar type-check-Lvar-class type-check-var-mixin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Integers and Variables                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-var-mixin (the parts reusable for Lvar and Cvar)

(define (type-check-var-mixin super-class)
  (class super-class
    (super-new)
    
    (define/public (operator-types)
      '((+ . ((Integer Integer) . Integer))
        (- . ((Integer Integer) . Integer))
        (read . (() . Integer))))

    (define/public (type-equal? t1 t2) (equal? t1 t2))

    (define/public (check-type-equal? t1 t2 e)
      (unless (type-equal? t1 t2)
        (error 'type-check "~a != ~a\nin ~v" t1 t2 e)))

    (define/public (type-check-op op arg-types e)
      (match (dict-ref (operator-types) op)
        [`(,param-types . ,return-type)
         (for ([at arg-types] [pt param-types])
           (check-type-equal? at pt e))
         return-type]
        [else (error 'type-check-op "unrecognized ~a" op)]))

    (define/public (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "Lvar ~a" e)
        (match e
          [(Var x)  (values (Var x) (dict-ref env x))]
          [(Int n)  (values (Int n) 'Integer)]
          [(Prim op es)
           (define-values (new-es ts)
             (for/lists (exprs types) ([e es]) ((type-check-exp env) e)))
           (values (Prim op new-es) (type-check-op op ts e))]
          [else (error 'type-check-exp "couldn't match ~a" e)])))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Lvar

(define type-check-Lvar-class
  (class (type-check-var-mixin object%)
    (super-new)
    (inherit check-type-equal?)

    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "Lvar ~a" e)
        (match e
          [(Let x e body)
           (define-values (e^ Te) ((type-check-exp env) e))
           (define-values (b Tb) ((type-check-exp (dict-set env x Te)) body))
           (values (Let x e^ b) Tb)]
          [else ((super type-check-exp env) e)])))

    (define/public (type-check-program e)
      (match e
        [(Program info body)
         (define-values (body^ Tb) ((type-check-exp '()) body))
         (check-type-equal? Tb 'Integer body)
         (Program info body^)]
        [else (error 'type-check-Lvar "couldn't match ~a" e)]))
    ))

(define (type-check-Lvar p)
  (send (new type-check-Lvar-class) type-check-program p))


