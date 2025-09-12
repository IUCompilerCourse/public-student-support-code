#lang racket
(require "utilities.rkt" "type_check_Lvar.rkt")
(provide type_check_Cvar type_check_Cvar-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Cvar

(define type_check_Cvar-class
  (class (type_check_var-mixin object%)
    (super-new)
    (inherit type_check_op type-equal? check-type-equal? type_check_exp)

    (define/public ((type_check_atm env) e)
      (match e
        [(Var x)  (values (Var x) (dict-ref env x))]
        [(Int n)  (values (Int n) 'Integer)]
        [else (error 'type_check_atm "expected a Cvar atm, not ~a" e)]))
      
    (define/public ((type_check_stmt env) s)
      (debug 'type_check_stmt "Cvar ~a" s)
      (match s
        [(Assign (Var x) e)
         (define-values (e^ t) ((type_check_exp env) e))
         (cond [(dict-has-key? env x)
                (check-type-equal? t (dict-ref env x) s)]
               [else (dict-set! env x t)])]
        [else (error 'type_check_stmt "expected a Cvar stmt, not ~a" s)]))

    (define/public ((type_check_tail env block-env blocks) t)
      (debug 'type_check_tail "Cvar ~a ~a" t env)
      (match t
        [(Return e)
         (define-values (e^ t) ((type_check_exp env) e))
         t]
        [(Seq s t)
         ((type_check_stmt env) s)
         ((type_check_tail env block-env blocks) t)]
        [else (error 'type_check_tail "expected a Cvar tail, not ~a" t)]))

    (define/public (type_check_program p)
      (match p
        [(CProgram info blocks)
         (define env (make-hash))
         (define block-env (make-hash))
         (define t ((type_check_tail env block-env blocks)
                    (dict-ref blocks 'start)))
         (unless (type-equal? t 'Integer)
           (error "return type of program must be Integer, not" t))
         (define locals-types (for/list ([(x t) (in-dict env)])
                                (cons x t)))
         (define new-info (dict-set info 'locals-types locals-types))
         (CProgram new-info blocks)]
        [else (error 'type_check_program "expected a C program, not ~a" p)]))
    ))

(define (type_check_Cvar p)
  (send (new type_check_Cvar-class) type_check_program p))
