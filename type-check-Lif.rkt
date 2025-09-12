#lang racket
(require "utilities.rkt")
(require "type_check_Lvar.rkt")
(provide type_check_Lif type_check_Lif-class type_check_if-mixin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Booleans and Control Flow                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_if-mixin (reusable for Lif and Cif)

(define (type_check_if-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal?)
    
    (define/override (type-equal? t1 t2)
      (debug 'type-equal? "lenient" t1 t2)
      (match* (t1 t2)
        [('_ t2) #t]
        [(t1 '_) #t]
        [(other wise) (super type-equal? t1 t2)]))
    
    (define/public (combine-types t1 t2)
      (match (list t1 t2)
        [(list '_ t2) t2]
        [(list t1 '_) t1]
        [else
         t1]))
    
    (define/override (operator-types)
      (append '((and . ((Boolean Boolean) . Boolean))
                (or . ((Boolean Boolean) . Boolean))
                (< . ((Integer Integer) . Boolean))
                (<= . ((Integer Integer) . Boolean))
                (> . ((Integer Integer) . Boolean))
                (>= . ((Integer Integer) . Boolean))
                (not . ((Boolean) . Boolean))
                )
              (super operator-types)))

    (define/override (type_check_exp env)
      (lambda (e)
        (debug 'type_check_exp "Lif" e)
        (match e
          [(Bool b) (values (Bool b) 'Boolean)]
          [(Prim 'eq? (list e1 e2))
           (define-values (e1^ T1) ((type_check_exp env) e1))
           (define-values (e2^ T2) ((type_check_exp env) e2))
           (check-type-equal? T1 T2 e)
           (values (Prim 'eq? (list e1^ e2^)) 'Boolean)]
          [else ((super type_check_exp env) e)])))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Lif

(define type_check_Lif-class
  (class (type_check_if-mixin type_check_Lvar-class)
    (super-new)
    (inherit check-type-equal? combine-types)

    (define/override (type_check_exp env)
      (lambda (e)
        (match e
          [(If cnd thn els)
           (define-values (cnd^ Tc) ((type_check_exp env) cnd))
           (define-values (thn^ Tt) ((type_check_exp env) thn))
           (define-values (els^ Te) ((type_check_exp env) els))
           (check-type-equal? Tc 'Boolean e)
           (check-type-equal? Tt Te e)
           (values (If cnd^ thn^ els^) (combine-types Tt Te))]
          [else ((super type_check_exp env) e)])))
    ))

(define (type_check_Lif p)
  (send (new type_check_Lif-class) type_check_program p))

