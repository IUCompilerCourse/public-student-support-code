#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Cif.rkt")
(require "type-check-Cvec.rkt")
(require "type-check-Rfun.rkt")
(provide type-check-Cfun type-check-Cfun-mixin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cfun

(define (type-check-Cfun-mixin super-class)
  (class super-class
    (super-new)
    (inherit type-check-exp type-equal? type-check-apply fun-def-type)

    (define/override ((type-check-tail env block-env G) t)
      (match t
        [(TailCall f arg*)
         (define-values (f^ arg*^ rt) (type-check-apply env f arg*))
         rt]
        [else ((super type-check-tail env block-env G) t)]
        ))

(define/override (type-check-def global-env)
  (lambda (d)
    (match d
      [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info blocks)
       (define new-env (append (map cons xs ps) global-env))
       (define env (make-hash new-env))
       (define block-env (make-hash))
       (define t ((type-check-tail env block-env blocks)
                  (dict-ref blocks (symbol-append f 'start))))
       (unless (type-equal? t rt)
         (error 'type-check "mismatch in return type, ~a != ~a" t rt))
       (define locals-types
         (for/list ([(x t) (in-dict env)]
                    #:when (not (dict-has-key? global-env x)))
           (cons x t)))
       (define new-info (dict-set info 'locals-types locals-types))
       (Def f p:t* rt new-info blocks)]
      )))

))

(define type-check-Cfun-class (type-check-Cfun-mixin
                             (type-check-Cvec-mixin
                              (type-check-Cif-mixin
                               (type-check-Cvar-mixin
                                type-check-Rfun-class)))))

(define (type-check-Cfun p)
  (send (new type-check-Cfun-class) type-check-program p))
