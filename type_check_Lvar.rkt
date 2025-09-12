#lang racket
(require "utilities.rkt")
(provide type_check_Lvar type_check_Lvar-class type_check_var-mixin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Integers and Variables                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_var-mixin (the parts reusable for Lvar and Cvar)

(define (type_check_var-mixin super-class)
  (class super-class
    (super-new)
    
    (define/public (operator-types)
      '((+ . ((Integer Integer) . Integer))
        (- . ((Integer Integer) . Integer))
        (read . (() . Integer))))

    (define/public (type-equal? t1 t2) (equal? t1 t2))

    (define/public (check-type-equal? t1 t2 e)
      (unless (type-equal? t1 t2)
        (error 'type_check "~a != ~a\nin ~v" t1 t2 e)))

    (define/public (type_check_op op arg-types e)
      (match (dict-ref (operator-types) op)
        [`(,param-types . ,return-type)
         (for ([at arg-types] [pt param-types])
           (check-type-equal? at pt e))
         return-type]
        [else (error 'type_check_op "unrecognized ~a" op)]))

    (define/public (type_check_exp env)
      (lambda (e)
        (debug 'type_check_exp "Lvar ~a" e)
        (match e
          [(Var x)  (values (Var x) (dict-ref env x))]
          [(Int n)  (values (Int n) 'Integer)]
          [(Prim op es)
           (define-values (new-es ts)
             (for/lists (exprs types) ([e es]) ((type_check_exp env) e)))
           (values (Prim op new-es) (type_check_op op ts e))]
          [else (error 'type_check_exp "couldn't match ~a" e)])))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Lvar

(define type_check_Lvar-class
  (class (type_check_var-mixin object%)
    (super-new)
    (inherit check-type-equal?)

    (define/override (type_check_exp env)
      (lambda (e)
        (debug 'type_check_exp "Lvar ~a" e)
        (match e
          [(Let x e body)
           (define-values (e^ Te) ((type_check_exp env) e))
           (define-values (b Tb) ((type_check_exp (dict-set env x Te)) body))
           (values (Let x e^ b) Tb)]
          [else ((super type_check_exp env) e)])))

    (define/public (type_check_program e)
      (match e
        [(Program info body)
         (define-values (body^ Tb) ((type_check_exp '()) body))
         (check-type-equal? Tb 'Integer body)
         (Program info body^)]
        [else (error 'type_check_Lvar "couldn't match ~a" e)]))
    ))

(define (type_check_Lvar p)
  (send (new type_check_Lvar-class) type_check_program p))


