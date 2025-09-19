#lang racket
(require "utilities.rkt")
(require "type_check_Lvec.rkt")
(require "type_check_Lvecof.rkt")
(provide type_check_Lfun type_check_Lfun_has_type
         type_check_Lfun-class type_check_fun-mixin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_fun-mixin (for use in Lfun and Cfun)

(define (type_check_fun-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal?)
    
    (define/override (type-equal? t1 t2)
      (match* (t1 t2)
        [(`(,ts1 ... -> ,rt1) `(,ts2 ... -> ,rt2))
         (and (for/and ([t1 ts1] [t2 ts2])
                (type-equal? t1 t2))
              (type-equal? rt1 rt2))]
        [(other wise) (super type-equal? t1 t2)]))
    
    (define/public (type_check_apply env e es)
      (define-values (e^ ty) ((type_check_exp env) e))
      (define-values (e* ty*) (for/lists (e* ty*) ([e (in-list es)])
                                ((type_check_exp env) e)))
      (match ty
        [`(,ty^* ... -> ,rt)
         (for ([arg-ty ty*] [param-ty ty^*])
           (check-type-equal? arg-ty param-ty (Apply e es)))
         (values e^ e* rt)]
        [else (error 'type_check "expected a function, not ~a" ty)]))

    (define/public (fun-def-type d)
      (match d [(Def f (list `[,xs : ,ps] ...) rt info body)  `(,@ps -> ,rt)]
        [else (error 'type_check "ill-formed function definition in ~a" d)]))

    (define/override (type_check_exp env)
      (lambda (e)
        (match e
          [(FunRef f n)
           (values (FunRef f n)  (dict-ref env f))]
          [(Call e es)
           (define-values (e^ es^ rt) (type_check_apply env e es))
           (values (Call e^ es^) rt)]
          [else ((super type_check_exp env) e)]
          )))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Lfun

;; TODO: Don't allow eq? on function types. -Jeremy

(define type_check_Lfun-class
  (class (type_check_fun-mixin type_check_Lvecof-class)
    (super-new)
    (inherit check-type-equal? type_check_apply fun-def-type)

    (field [max-parameters 32])

    (define/override (type_check_exp env)
      (lambda (e)
        (match e
          [(Apply e es)
           (define-values (e^ es^ rt) (type_check_apply env e es))
           (values (Apply e^ es^) rt)]
          [else ((super type_check_exp env) e)]
          )))

    (define/public (type_check_def env)
      (lambda (e)
        (match e
          [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info body)
           (unless (< (length xs) max-parameters)
             (error 'type_check "~a has too many parameters, max is ~a"
                    f max-parameters))
           (define new-env (append (map cons xs ps) env))
           (define-values (body^ ty^) ((type_check_exp new-env) body))
           (check-type-equal? ty^ rt body)
           (Def f p:t* rt info body^)]
          [else (error 'type_check "ill-formed function definition ~a" e)]
          )))	 

    (define/override (type_check_program e)
      (match e
        [(ProgramDefsExp info ds body)
         (define new-env (for/list ([d ds])
                           (cons (Def-name d) (fun-def-type d))))
         (define ds^ (for/list ([d ds]) ((type_check_def new-env) d)))
         (define-values (body^ ty) ((type_check_exp new-env) body))
         (check-type-equal? ty 'Integer body)
         (ProgramDefsExp info ds^ body^)]
        [(ProgramDefs info ds)
         (define new-env (for/list ([d ds]) 
                           (cons (Def-name d) (fun-def-type d))))
         (define ds^ (for/list ([d ds]) ((type_check_def new-env) d)))
         ;; TODO: check that main has Integer return type.
         (ProgramDefs info ds^)]
        [(Program info body)
         (define-values (body^ ty) ((type_check_exp '()) body))
         (check-type-equal? ty 'Integer body)
         (ProgramDefsExp info '() body^)]
        [else (error 'type_check "unrecognized ~a" e)]))
    ))

(define (type_check_Lfun p)
  (send (new type_check_Lfun-class) type_check_program p))

(define (type_check_Lfun_has_type p)
  (begin
    (typed-vec #t)
    (typed-vecof #t)
    (define t (type_check_Lfun p))
    (typed-vec #f)
    (typed-vecof #f)
    t))

