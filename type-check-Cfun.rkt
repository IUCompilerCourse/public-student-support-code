#lang racket
(require "utilities.rkt")
(require "type-check-Cvecof.rkt")
(require "type-check-Lfun.rkt")
(provide type-check-Cfun type-check-Cfun-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cfun

(define type-check-Cfun-class
  (class (type-check-fun-mixin type-check-Cvecof-class)
    (super-new)
    (inherit type-equal? type-check-apply type-check-blocks fun-def-type
             exp-ready?)
    
    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(FunRef f n) (set)]
	[(Apply e es)
	 (apply set-union (cons (recur e) (map recur es)))]
        [(Call f arg*) (apply set-union (cons (recur f) (map recur arg*)))]
        [else (super free-vars-exp e)]))
    
    (define/override ((type-check-tail env block-env G) t)
      (debug 'type-check-tail "Cfun" t)
      (match t
        [(TailCall f arg*)
         #:when (and (exp-ready? f env)
                     (for/and ([arg arg*]) (exp-ready? arg env)))
         (define-values (f^ arg*^ rt) (type-check-apply env f arg*))
         rt]
        [(TailCall f arg*) '_]
        [else ((super type-check-tail env block-env G) t)]
        ))

    (define/override ((type-check-stmt env) s)
      (match s
        [(Call e es)
         #:when (and (exp-ready? e env)
                     (for/and ([arg es]) (exp-ready? arg env)))
         (define-values (e^ es^ rt) (type-check-apply env e es))
         (void)]
        [else ((super type-check-stmt env) s)]))
    
    (define/public (type-check-def global-env)
      (lambda (d)
        (match d
          [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info blocks)
           (define new-env (append (map cons xs ps) global-env))
           (define env^ (make-hash new-env))
           (define-values (env t)
             (type-check-blocks info blocks env^ (symbol-append f '_start)))
           (unless (type-equal? t rt)
             (error 'type-check "mismatch in return type, ~a != ~a" t rt))
           (define locals-types
             (for/list ([(x t) (in-dict env)]
                        #:when (not (dict-has-key? global-env x)))
               (cons x t)))
           (define new-info (dict-set info 'locals-types locals-types))
           (Def f p:t* rt new-info blocks)]
          )))

    (define/override (type-check-program p)
      (match p
        [(ProgramDefs info ds)
         (define new-env (for/list ([d ds]) 
                           (cons (Def-name d) (fun-def-type d))))
         (define ds^ (for/list ([d ds])
                       ((type-check-def new-env) d)))
         (ProgramDefs info ds^)]
        [else (error 'type-check-program "expected a C program, not ~a" p)]
        ))
    ))

(define (type-check-Cfun p)
  (send (new type-check-Cfun-class) type-check-program p))
