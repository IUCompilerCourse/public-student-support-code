#lang racket
(require "utilities.rkt")
(require "type_check_Cif.rkt")
(provide type_check_Cwhile type_check_Cwhile-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Cwhile

(define type_check_Cwhile-class
  (class type_check_Cif-class
    (super-new)
    (inherit check-type-equal?)

    #;(define/public (combine-types t1 t2)
      (match (list t1 t2)
        [(list '_ t2) t2]
        [(list t1 '_) t1]
        [else
         t1]))

    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(WhileLoop cnd body)
         (set-union (recur cnd) (recur body))]
        [(Begin es e)
         (apply set-union (cons (recur e) (map recur es)))]
        [(SetBang x rhs) (set-union (set x) (recur rhs))]
        ;; C-level expressions
        [(Void) (set)]
	[else (super free-vars-exp e)]))
    

    #;(define (update-type x t env)
      (debug 'update-type x t)
      (cond [(dict-has-key? env x)
             (define old-t (dict-ref env x))
             (unless (type-equal? t old-t)
               (error 'update-type "old type ~a and new type ~ are inconsistent"
                      old-t t))
             (define new-t (combine-types old-t t))
             (cond [(not (equal? new-t old-t))
                    (dict-set! env x new-t)
                    (set! type-changed #t)])]
            [(eq? t '_)
             (void)]
            [else
             (set! type-changed #t)
             (dict-set! env x t)]))

    (define/override ((type_check_atm env) e)
      (match e
        [(Void) (values (Void) 'Void)]
        [else
         ((super type_check_atm env) e)]
        ))
    
    (define/override (type_check_exp env)
      (lambda (e)
        (debug 'type_check_exp "Cwhile" e)
        (define recur (type_check_exp env))
        (match e
          [(Void) (values (Void) 'Void)]
          [else ((super type_check_exp env) e)])))
    
    ))

(define (type_check_Cwhile p)
  (send (new type_check_Cwhile-class) type_check_program p))
  
