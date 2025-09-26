#lang racket
(require "utilities.rkt")
(require "type_check_Cvecof.rkt")
(require "type_check_Lfun.rkt")
(provide type_check_Cfun type_check_Cfun-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Cfun

(define type_check_Cfun-class
  (class (type_check_fun-mixin type_check_Cvecof-class)
    (super-new)
    (inherit type-equal? type_check_apply type_check_blocks fun-def-type
             exp-ready?)
    
    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(FunRef f n) (set)]
	[(Apply e es)
	 (apply set-union (cons (recur e) (map recur es)))]
        [(Call f arg*) (apply set-union (cons (recur f) (map recur arg*)))]
        [else (super free-vars-exp e)]))
    
    (define/override ((type_check_tail env block-env G) t)
      (debug 'type_check_tail "Cfun" t)
      (match t
        [(TailCall f arg*)
         #:when (and (exp-ready? f env)
                     (for/and ([arg arg*]) (exp-ready? arg env)))
         (define-values (f^ arg*^ rt) (type_check_apply env f arg*))
         rt]
        [(TailCall f arg*) '_]
        [else ((super type_check_tail env block-env G) t)]
        ))

    (define/override ((type_check_stmt env) s)
      (match s
        [(Call e es)
         #:when (and (exp-ready? e env)
                     (for/and ([arg es]) (exp-ready? arg env)))
         (define-values (e^ es^ rt) (type_check_apply env e es))
         (void)]
        [else ((super type_check_stmt env) s)]))
    
    (define/public (type_check_def global-env)
      (lambda (d)
        (match d
          [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info blocks)
           (define new-env (append (map cons xs ps) global-env))
           (define env^ (make-hash new-env))
           (define-values (env t)
             (type_check_blocks info blocks env^ (symbol-append f '_start)))
           (unless (type-equal? t rt)
             (error 'type_check "mismatch in return type, ~a != ~a" t rt))
           (define locals-types
             (for/list ([(x t) (in-dict env)]
                        #:when (not (dict-has-key? global-env x)))
               (cons x t)))
           (define new-info (dict-set info 'locals-types locals-types))
           (Def f p:t* rt new-info blocks)]
          )))

    (define/override (type_check_program p)
      (match p
        [(ProgramDefs info ds)
         (define new-env (for/list ([d ds]) 
                           (cons (Def-name d) (fun-def-type d))))
         (define ds^ (for/list ([d ds])
                       ((type_check_def new-env) d)))
         (ProgramDefs info ds^)]
        [else (error 'type_check_program "expected a C program, not ~a" p)]
        ))
    ))

(define (type_check_Cfun p)
  (send (new type_check_Cfun-class) type_check_program p))
