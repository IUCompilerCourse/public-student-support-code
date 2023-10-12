#lang racket
(require "utilities.rkt")
(require "type-check-Lvec.rkt")
(require "type-check-Lvecof.rkt")
(require "type-check-Llambda.rkt")
(provide type-check-Lany type-check-Lany-has-type
         type-check-Lany-class type-check-any-mixin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Type Checker for the Any type and inject, project, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (type-check-any-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal?)
    
    (define/override (operator-types)
      (append
       '((integer? . ((Any) . Boolean))
         (boolean? . ((Any) . Boolean))
         (vector? . ((Any) . Boolean))
         (procedure? . ((Any) . Boolean))
         (void? . ((Any) . Boolean))
         (tag-of-any . ((Any) . Integer))
         (make-any . ((_ Integer) . Any))
         )
       (super operator-types)))
    
    (define/public (join-types t1 t2)
      (match (list t1 t2)
        [(list '_ t2) t2]
        [(list t1 '_) t1]
        [(list `(Vector ,ts1 ...)
               `(Vector ,ts2 ...))
         `(Vector ,@(for/list ([t1 ts1] [t2 ts2])
                      (join-types t1 t2)))]
        [(list `(,ts1 ... -> ,rt1)
               `(,ts2 ... -> ,rt2))
         `(,@(for/list ([t1 ts1] [t2 ts2])
               (join-types t1 t2))
           -> ,(join-types rt1 rt2))]
        [else
         t1]))

    (define/public (flat-ty? ty)
      (match ty
        [(or `Integer `Boolean '_ `Void)
         #t]
        ;; The following is a special case to handle programs
        ;; after closure conversion. -Jeremy
        [`(Vector ((Vector _) ,ts ... -> Any))
         (for/and ([t ts]) (eq? t 'Any))]
        [`(Vector ,ts ...)
         (for/and ([t ts]) (eq? t 'Any))]
        ['(Vectorof Any) #t]
        [`(,ts ... -> ,rt)
         (and (eq? rt 'Any) (for/and ([t ts]) (eq? t 'Any)))]
        [else
         #f]
        ))

    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "any" e)
        (define recur (type-check-exp env))
        (match e
          [(Prim 'any-vector-length (list e1))
           (define-values (e1^ t1) (recur e1))
           (check-type-equal? t1 'Any e)
           (values (Prim 'any-vector-length (list e1^)) 'Integer)]
          [(Prim 'any-vectorof-length (list e1))
           (define-values (e1^ t1) (recur e1))
           (check-type-equal? t1 'Any e)
           (values (Prim 'any-vectorof-length (list e1^)) 'Integer)]
          [(Prim 'any-vector-ref (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (check-type-equal? t1 'Any e)
           (check-type-equal? t2 'Integer e)
           (values (Prim 'any-vector-ref (list e1^ e2^)) 'Any)]
          [(Prim 'any-vectorof-ref (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (check-type-equal? t1 'Any e)
           (check-type-equal? t2 'Integer e)
           (values (Prim 'any-vectorof-ref (list e1^ e2^)) 'Any)]
          [(Prim 'any-vector-set! (list e1 e2 e3))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (define-values (e3^ t3) (recur e3))
           (check-type-equal? t1 'Any e)
           (check-type-equal? t2 'Integer e)
           (check-type-equal? t3 'Any e)
           (values (Prim 'any-vector-set! (list e1^ e2^ e3^)) 'Void)]
          [(Prim 'any-vectorof-set! (list e1 e2 e3))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (define-values (e3^ t3) (recur e3))
           (check-type-equal? t1 'Any e)
           (check-type-equal? t2 'Integer e)
           (check-type-equal? t3 'Any e)
           (values (Prim 'any-vectorof-set! (list e1^ e2^ e3^)) 'Void)]
          [(ValueOf e ty)
           (define-values (new-e e-ty) (recur e))
           (values (ValueOf new-e ty) ty)]
          [else ((super type-check-exp env) e)])))
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Lany

(define type-check-Lany-class
  (class (type-check-any-mixin type-check-Llambda-class)
    (super-new)
    (inherit check-type-equal? join-types operator-types flat-ty?)

    (define/public (type-predicates)
      (set 'boolean? 'integer? 'vector? 'procedure? 'void?))

    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "Lany" e)
        (define recur (type-check-exp env))
        (match e
          ;; Change If to use join-types
          [(If cnd thn els)
           (define-values (cnd^ Tc) (recur cnd))
           (define-values (thn^ Tt) (recur thn))
           (define-values (els^ Te) (recur els))
           (check-type-equal? Tc 'Boolean cnd)
           (check-type-equal? Tt Te e)
           (values (If cnd^ thn^ els^) (join-types Tt Te))]
          [(Inject e1 ty)
           (unless (flat-ty? ty)
             (error 'type-check "may only inject from flat type, not ~a" ty))
           (define-values (new-e1 e-ty) (recur e1))
           (check-type-equal? e-ty ty e)
           (values (Inject new-e1 ty) 'Any)]
          [(Project e1 ty)
           (unless (flat-ty? ty)
             (error 'type-check "may only project to flat type, not ~a" ty))
           (define-values (new-e1 e-ty) (recur e1))
           (check-type-equal? e-ty 'Any e)
           (values (Project new-e1 ty) ty)]
          [(Prim pred (list e1))
           #:when (set-member? (type-predicates) pred)
           (define-values (new-e1 e-ty) (recur e1))
           (check-type-equal? e-ty 'Any e)
           (values (Prim pred (list new-e1)) 'Boolean)]
          [(Prim 'eq? (list arg1 arg2))
           (define-values (e1 t1) (recur arg1))
           (define-values (e2 t2) (recur arg2))
           (match* (t1 t2)
             ;; allow comparison of vectors of different element types
             [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))   (void)]
             [(`(Vectorof ,t1) `(Vectorof ,t2))         (void)]
             [(other wise) (check-type-equal? t1 t2 e)])
           (values (Prim 'eq? (list e1 e2)) 'Boolean)]
          [else ((super type-check-exp env) e)])))

    ))

(define (type-check-Lany p)
  (send (new type-check-Lany-class) type-check-program p))

(define (type-check-Lany-has-type p)
  (begin
    (typed-vec #t)
    (typed-vecof #t)
    (define t (send (new type-check-Lany-class) type-check-program p))
    (typed-vec #f)
    (typed-vecof #f)
    t))
