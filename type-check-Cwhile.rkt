#lang racket
;(require graph)
;(require "multigraph.rkt")
(require "utilities.rkt")
(require "type-check-Lwhile.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Cif.rkt")
(provide type-check-Cwhile type-check-Cwhile-mixin type-check-Cwhile-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cwhile

(define (type-check-Cwhile-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal?)

    #;(define/public (combine-types t1 t2)
      (match (list t1 t2)
        [(list '_ t2) t2]
        [(list t1 '_) t1]
        [else
         t1]))

    ;; TODO: move some things from here to later type checkers
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

    (define/override ((type-check-atm env) e)
      (match e
        [(Void) (values (Void) 'Void)]
        [else
         ((super type-check-atm env) e)]
        ))
    
    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "Cwhile" e)
        (define recur (type-check-exp env))
        (match e
          [(Void) (values (Void) 'Void)]
          [else ((super type-check-exp env) e)])))
    
    
    #;(define/override (type-check-program p)
      (match p
        [(CProgram info blocks)
         (define empty-env (make-hash))
         (define-values (env t)
           (type-check-blocks info blocks empty-env 'start))
         (unless (type-equal? t 'Integer)
           (error "return type of program must be Integer, not" t))
         (define locals-types
           (for/list ([(x t) (in-dict env)])
             (cons x t)))
         (define new-info (dict-set info 'locals-types locals-types))
         (CProgram new-info blocks)]
        [else (super type-check-program p)]))

    ))

(define type-check-Cwhile-class (type-check-Cwhile-mixin
                                 (type-check-Cif-mixin
                                  (type-check-Cvar-mixin
                                   type-check-Lwhile-class))))

(define (type-check-Cwhile p)
  (send (new type-check-Cwhile-class) type-check-program p))
  
