#lang racket
(require "utilities.rkt")
(require "type-check-Llambda.rkt")

(provide type-check-poly type-check-poly-class)

(define type-check-poly-class
  (class type-check-Llambda-class
    (super-new)
    (inherit check-type-equal?)

    ;; This is slow. Should instead convert to de Bruijn
    ;; or add a parameter for the environment. -Jeremy
    (define/override (type-equal? t1 t2)
      (match* (t1 t2)
        [(`(All ,xs ,T1) `(All ,ys ,T2))
         (define env (map cons xs ys))
         (type-equal? (subst-type env T1) T2)]
        [(other wise)
         (super type-equal? t1 t2)]))
    
    (define/public (match-types env param_ty arg_ty)
      (verbose 'type-check "match-types" env param_ty arg_ty)
      (define result
      (match* (param_ty arg_ty)
        [('Integer 'Integer) env]
        [('Boolean 'Boolean) env]
        [('Void 'Void) env]
        [('Any 'Any) env]
        [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
         (for/fold ([env^ env]) ([pat1 ts1] [t2 ts2])
           (match-types env^ pat1 t2))]
        [(`(,ts1 ... -> ,rt1) `(,ts2 ... -> ,rt2))
         (define env^ (match-types env rt1 rt2))
         (for/fold ([env^^ env^]) ([pat1 ts1] [t2 ts2])
           (match-types env^^ pat1 t2))]
        [(`(All ,xs1 ,t1) `(All ,xs2 ,t2))
         (define env^ (append (map cons xs1 xs2) env))
         (match-types env^ t1 t2)]
        [((? symbol? x) t)
         (match (dict-ref env x (lambda () #f))
           [#f (error 'type-check "undefined type variable ~a" x)]
           ['Type (cons (cons x t) env)]
           [t^ (check-type-equal? t t^ 'matching) env])]
        [(other wise) (error 'type-check "mismatch ~a != a" param_ty arg_ty)]))
      (copious 'match-types "done" param_ty arg_ty result)
      result)

    (define/public (subst-type env pat1)
      (verbose 'type-check "subst" env pat1)
      (match pat1
        ['Integer 'Integer]
        ['Boolean 'Boolean]
        ['Void 'Void]
        ['Any 'Any]
        [`(Vector ,ts ...)
         `(Vector ,@(for/list ([t ts]) (subst-type env t)))]
        [`(,ts ... -> ,rt)
         `(,@(for/list ([t ts]) (subst-type env t)) -> ,(subst-type env rt))]
        [`(All ,xs ,t)
         `(All ,xs ,(subst-type (append (map cons xs xs) env) t))]
        [(? symbol? x) (dict-ref env x)]
        [else (error 'type-check "expected a type not ~a" pat1)]))
    
    (define/override (fun-def-type d)
      (match d
        [(Def f (list `[,xs : ,ps] ...) rt info body)  `(,@ps -> ,rt)]
        [(Poly ts (Def f (list `[,xs : ,ps] ...) rt info body))
         `(All ,ts (,@ps -> ,rt))]
        [else (error 'fun-def-type "expected function definition, not ~a" d)]))

    (define/public (def-name d)
      (match d
        [(Def f params rt info body) f]
        [(Poly ts (Def f params rt info body)) f]))
    
    (define/public ((check-well-formed env) ty)
      (match ty
        ['Integer (void)]
        ['Boolean (void)]
        ['Void (void)]
        ['Any (void)]
        [(? symbol? a)
         (match (dict-ref env a (lambda () #f))
           ['Type (void)]
           [else (error 'type-check "undefined type variable ~a" a)])]
        [`(Vector ,ts ...)
         (for ([t ts]) ((check-well-formed env) t))]
        [`(,ts ... -> ,t)
         (for ([t ts]) ((check-well-formed env) t))
         ((check-well-formed env) t)]
        [`(All ,xs ,t)
         (define env^ (append (for/list ([x xs]) (cons x 'Type)) env))
         ((check-well-formed env^) t)]
        [else (error 'type-check "unrecognized type ~a" ty)]))
    
    (define/public (combine-decls-defs ds)
      (match ds
        ['() '()]
        [`(,(Decl name type) . (,(Def f params _ info body) . ,ds^))
         (unless (equal? name f)
           (error 'type-check "name mismatch, ~a != ~a" name f))
         (match type
           [`(All ,xs (,ps ... -> ,rt))
            (define params^ (for/list ([x params] [T ps]) `[,x : ,T]))
            (cons (Poly xs (Def name params^ rt info body))
                  (combine-decls-defs ds^))]
           [`(,ps ... -> ,rt)
            (define params^ (for/list ([x params] [T ps]) `[,x : ,T]))
            (cons (Def name params^ rt info body) (combine-decls-defs ds^))]
           [else (error 'type-check "expected a function type, not ~a" type) ])]
        [`(,(Def f params rt info body) . ,ds^)
         (cons (Def f params rt info body) (combine-decls-defs ds^))]))

    (define/override (type-check-apply env e1 es)
      (define-values (e^ ty) ((type-check-exp env) e1))
      (define-values (es^ ty*) (for/lists (es^ ty*) ([e (in-list es)])
                                ((type-check-exp env) e)))
      (match ty
        [`(,ty^* ... -> ,rt)
         (for ([arg-ty ty*] [param-ty ty^*])
           (check-type-equal? arg-ty param-ty (Apply e1 es)))
         (values e^ es^ rt)]
        [`(All ,xs (,tys ... -> ,rt))
         (define env^ (append (for/list ([x xs]) (cons x 'Type)) env))
         (define env^^ (for/fold ([env^^ env^]) ([arg-ty ty*] [param-ty tys])
                         (match-types env^^ param-ty arg-ty)))
         (debug 'type-check "match result" env^^)
         (define targs
           (for/list ([x xs])
             (match (dict-ref env^^ x (lambda () #f))
               [#f (error 'type-check "type variable ~a not deduced\nin ~v"
                          x (Apply e1 es))]
               [ty ty])))
         (values (Inst e^ ty targs) es^ (subst-type env^^ rt))]
        [else (error 'type-check "expected a function, not ~a" ty)]))
    
    (define/override ((type-check-exp env) e)
      (verbose 'type-check "poly/exp begin" e env)
      (define-values (e^ ty)
        (match e
          [(Lambda `([,xs : ,Ts] ...) rT body)
           (for ([T Ts]) ((check-well-formed env) T))
           ((check-well-formed env) rT)
           ((super type-check-exp env) e)]
          [(HasType e1 ty)
           ((check-well-formed env) ty)
           ((super type-check-exp env) e)]
          [else ((super type-check-exp env) e)]))
      (verbose 'type-check "poly/exp end" e e^ ty)
      (values e^ ty))
    
    (define/override ((type-check-def env) d)
      (verbose 'type-check "poly/def" d)
      (match d
        [(Poly ts (Def f (and p:t* (list `[,xs : ,ps] ...)) rt info body))
         (define ts-env (for/list ([t ts]) (cons t 'Type)))
         (for ([p ps]) ((check-well-formed ts-env) p))
         ((check-well-formed ts-env) rt)
         (define new-env (append ts-env (map cons xs ps) env))
         (define-values (body^ ty^) ((type-check-exp new-env) body))
         (check-type-equal? ty^ rt body)
         (Poly ts (Def f p:t* rt info body^))]
        [else ((super type-check-def env) d)]))

    (define/override (type-check-program p)
      (verbose 'type-check "poly/program" p)
      (match p
        [(Program info body)
         (type-check-program (ProgramDefsExp info '() body))]
        [(ProgramDefsExp info ds body)
         (define ds^ (combine-decls-defs ds))
         (define new-env (for/list ([d ds^])
                           (cons (def-name d) (fun-def-type d))))
         (define ds^^ (for/list ([d ds^]) ((type-check-def new-env) d)))
         (define-values (body^ ty) ((type-check-exp new-env) body))
         (check-type-equal? ty 'Integer body)
         (ProgramDefsExp info ds^^ body^)]))
    
    ))

(define (type-check-poly p)
  (send (new type-check-poly-class) type-check-program p))
