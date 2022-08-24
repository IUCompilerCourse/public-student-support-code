#lang racket
(require "utilities.rkt")
(require "type-check-Lvecof.rkt")
(provide type-check-Lfun type-check-Lfun-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Lfun

;; TODO: Don't allow eq? on function types. -Jeremy

(define type-check-Lfun-class
  (class type-check-Lvecof-class
    (super-new)
    (inherit check-type-equal?)

    (field [max-parameters 32])

    ;; Need lenient checking for closure conversion.
    ;; Putting it here instead of in lambda because the C-level type
    ;; checkers also need it and inherit from this type checker.

    (define/override (type-equal? t1 t2)
      (match* (t1 t2)
        [(`(,ts1 ... -> ,rt1) `(,ts2 ... -> ,rt2))
         (and (for/and ([t1 ts1] [t2 ts2])
                (type-equal? t1 t2))
              (type-equal? rt1 rt2))]
        [(other wise) (super type-equal? t1 t2)]))
    
    (define/public (type-check-apply env e es)
      (define-values (e^ ty) ((type-check-exp env) e))
      (define-values (e* ty*) (for/lists (e* ty*) ([e (in-list es)])
                                ((type-check-exp env) e)))
      (match ty
        [`(,ty^* ... -> ,rt)
		  (let ([expr (Apply e es)])
			(for-each (lambda (at pt) (check-type-equal? at pt expr)) ty* ty^*))
		  (values e^ e* rt)]
        [else (error 'type-check "expected a function, not ~a" ty)]))

    (define/override (type-check-exp env)
      (lambda (e)
        (match e
          [(FunRef f n)
           (values (FunRef f n)  (dict-ref env f))]
          [(Apply e es)
           (define-values (e^ es^ rt) (type-check-apply env e es))
           (values (Apply e^ es^) rt)]
          [(Call e es)
           (define-values (e^ es^ rt) (type-check-apply env e es))
           (values (Call e^ es^) rt)]
          [else ((super type-check-exp env) e)]
          )))

    (define/public (type-check-def env)
      (lambda (e)
        (match e
          [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info body)
           (unless (< (length xs) max-parameters)
             (error 'type-check "~a has too many parameters, max is ~a"
                    f max-parameters))
           (define new-env (append (map cons xs ps) env))
           (define-values (body^ ty^) ((type-check-exp new-env) body))
           (check-type-equal? ty^ rt body)
           (Def f p:t* rt info body^)]
          [else (error 'type-check "ill-formed function definition ~a" e)]
          )))	 

    (define/public (fun-def-type d)
      (match d [(Def f (list `[,xs : ,ps] ...) rt info body)  `(,@ps -> ,rt)]
        [else (error 'type-check "ill-formed function definition in ~a" d)]))

    (define/override (type-check-program e)
      (match e
        [(ProgramDefsExp info ds body)
         (define new-env (for/list ([d ds])
                           (cons (Def-name d) (fun-def-type d))))
         (define ds^ (for/list ([d ds]) ((type-check-def new-env) d)))
         (define-values (body^ ty) ((type-check-exp new-env) body))
         (check-type-equal? ty 'Integer body)
         (ProgramDefsExp info ds^ body^)]
        [(ProgramDefs info ds)
         (define new-env (for/list ([d ds]) 
                           (cons (Def-name d) (fun-def-type d))))
         (define ds^ (for/list ([d ds]) ((type-check-def new-env) d)))
         ;; TODO: check that main has Integer return type.
         (ProgramDefs info ds^)]
        [(Program info body)
         (define-values (body^ ty) ((type-check-exp '()) body))
         (check-type-equal? ty 'Integer body)
         (ProgramDefsExp info '() body^)]
        [else (error 'type-check "unrecognized ~a" e)]))
    ))

(define (type-check-Lfun p)
  (send (new type-check-Lfun-class) type-check-program p))

