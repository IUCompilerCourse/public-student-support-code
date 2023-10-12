#lang racket
;(require graph)
;(require "multigraph.rkt")
(require "utilities.rkt")
(require "type-check-Lif.rkt")
(provide type-check-Lwhile type-check-Lwhile-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           while, begin, set!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define type-check-Lwhile-class
  (class type-check-Lif-class
    (super-new)
    (inherit check-type-equal?)

    ;; lenient type checking for '_
    (define/override (type-equal? t1 t2)
      (debug 'type-equal? "lenient" t1 t2)
      (match* (t1 t2)
        [('_ t2) #t]
        [(t1 '_) #t]
        [(other wise) (equal? t1 t2)]))
    
    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "Lwhile" e)
        (define recur (type-check-exp env))
        (match e
          [(SetBang x rhs)
           (define-values (rhs^ rhsT) (recur rhs))
           (define varT (dict-ref env x))
           (check-type-equal? rhsT varT e)
           (values (SetBang x rhs^) 'Void)]
          [(GetBang x)
           (values (GetBang x) (dict-ref env x))]
          [(WhileLoop cnd body)
           (define-values (cnd^ Tc) (recur cnd))
           (check-type-equal? Tc 'Boolean e)
           (define-values (body^ Tbody) ((type-check-exp env) body))
           (values (WhileLoop cnd^ body^) 'Void)]
          [(Begin es body)
           (define-values (es^ ts)
             (for/lists (l1 l2) ([e es]) (recur e)))
           (define-values (body^ Tbody) (recur body))
           (values (Begin es^ body^) Tbody)]
          [(Void) (values (Void) 'Void)]
          [else ((super type-check-exp env) e)])))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Lwhile

(define (type-check-Lwhile p)
  (send (new type-check-Lwhile-class) type-check-program p))

